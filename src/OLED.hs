module OLED where

import Clash.Prelude hiding (showChar)
import Calculator.Prelude
import Clash.Annotations.Primitive
import Data.Bifunctor (Bifunctor(bimap))

{-# OPAQUE oledDriver #-}
{-# ANN oledDriver hasBlackBox #-}
oledDriver
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Signal dom Bit  -- ^ Show char
  -> Signal dom Bit  -- ^ Clear display
  -> Signal dom (Unsigned 8)  -- ^ ASCII char value
  -> Signal dom (Unsigned 2)  -- ^ Row index
  -> Signal dom (Unsigned 4)  -- ^ Column index
  -> Signal dom (Unsigned 8)  -- ^ OLED pins out
oledDriver !_ !_ !_ !_ !_ !_ !_ = undefined

oled
  :: HiddenClockResetEnable DomMain
  => Signal DomMain (Maybe CalcValue)
  -> Signal DomMain (Unsigned 7)
oled bmData = bOLEDPinsOut
  where
    bOLEDResult = mealy transfer OLEDState
      { mHeldValue = Nothing
      , isWorkingOnNumerator = True
      , step = Collect
      } $ bundle (bmData, bEnabled)
    (bEnabled, bOLEDPinsOut) = unbundle $ bimap unpack unpack . split . pack <$> driver
      (showChar <$> bOLEDResult) (clearDisplay <$> bOLEDResult)
      (charValue <$> bOLEDResult) (rowIndex <$> bOLEDResult) (colIndex <$> bOLEDResult)
    driver = hideReset $ hideClock oledDriver

data OLEDStep = Collect | Clearing | BCD BCDState | Print PrintState
  deriving (Eq, Ord, Show, Generic, NFDataX)
data OLEDState = OLEDState
  { mHeldValue :: Maybe CalcValue
  , isWorkingOnNumerator :: Bool
  , step :: OLEDStep
  } deriving (Eq, Ord, Show, Generic, NFDataX)
data OLEDResult = OLEDResult
  { showChar :: Bit
  , clearDisplay :: Bit
  , charValue :: Unsigned 8
  , rowIndex :: Unsigned 2
  , colIndex :: Unsigned 4
  } deriving (Eq, Ord, Show, Generic, NFDataX)
defaultResult :: OLEDResult
defaultResult = OLEDResult 0 0 0 0 0
transfer :: OLEDState -> (Maybe CalcValue, Bool) -> (OLEDState, OLEDResult)
transfer state (mValue, isReady) = case step state of
  Collect -> if mValue == mHeldValue state
    then (state, defaultResult)
    else (,) OLEDState
      { mHeldValue = mValue
      , isWorkingOnNumerator = True
      , step = Clearing
      } defaultResult
      { clearDisplay = high }
  Clearing -> if isReady
    then (state { step = maybe Collect (BCD . initBCD valNumerator) (mHeldValue state) }, defaultResult)
    else (state, defaultResult)
  BCD bcd -> if iBCDStep (bcdStepState bcd) > 0
      then (state
        { step = BCD $ bcd { bcdStepState = transferBCD (bcdStepState bcd) }
        }, defaultResult)
      else (state
        { step = Print PrintState
          { iPrintCol = 0
          , waiting = False
          , readyRow = asciiFromScratchSpace $ bcdScratchSpace $ bcdStepState bcd
          , printValue = workingValue bcd
          }
        }, defaultResult)
  Print printState -> transferPrint isReady state printState

type ScratchSize = NBCDBits + NCalcValueBits
type ScratchSpace = BitVector ScratchSize
data BCDStepState = BCDStepState
  { iBCDStep :: Unsigned (1 + Log2 NCalcValueBits)
  , bcdScratchSpace :: ScratchSpace
  } deriving (Eq, Ord, Show, Generic, NFDataX)
data BCDState = BCDState
  { workingValue :: CalcValue
  , bcdStepState :: BCDStepState
  } deriving (Eq, Ord, Show, Generic, NFDataX)
initBCD :: (CalcValue -> Unsigned NCalcValueBits) -> CalcValue -> BCDState
initBCD getAtor value = BCDState
  { workingValue = value
  , bcdStepState = BCDStepState
    { iBCDStep = snatToNum (SNat :: SNat NCalcValueBits)
    , bcdScratchSpace = zeroExtend $ pack $ getAtor value
    }
  }
transferBCD :: BCDStepState -> BCDStepState
transferBCD BCDStepState{..} = BCDStepState (iBCDStep - 1) nextScratchSpace
  where
    bcdDestination :: BitVector NBCDBits
    binaryFeed :: BitVector NCalcValueBits
    (bcdDestination, binaryFeed) = split bcdScratchSpace
    -- Slightly modified from base 10 double dabble
    -- Add 2 if chunk is more than 5, rather than +3 when > 4
    bcdAdjust = withPacking $ \digitChunk -> if digitChunk > 5
      then digitChunk + 2
      else digitChunk
    scratchChunks :: Vec NBCDDigits (Vec 4 Bit)
    scratchChunks = map bcdAdjust $ unconcat (SNat :: SNat 4) $ unpack bcdDestination
    nextScratchSpace = (pack (concat scratchChunks) ++# binaryFeed) .<<+ 0

type ReadyRow = Vec NBCDDigits (Unsigned 8)
asciiFromScratchSpace :: ScratchSpace -> ReadyRow
asciiFromScratchSpace scratchSpace = bcdDigits ## \bcd -> let
  resized = bitCoerce $ zeroExtend $ pack bcd
  ascii = resized + 48
  in if ascii > 57
    then ascii + 7
    else ascii
  where
    bcdDigits :: Vec NBCDDigits (Vec 4 Bit)
    bcdDigits = unconcat (SNat :: SNat 4) $ takeI $ unpack scratchSpace
data PrintState = PrintState
  { printValue :: CalcValue
  , iPrintCol :: Index NBCDDigits
  , readyRow :: ReadyRow
  , waiting :: Bool
  } deriving (Eq, Ord, Show, Generic, NFDataX)
transferPrint :: Bool -> OLEDState -> PrintState -> (OLEDState, OLEDResult)
transferPrint isReady state printState@PrintState{..} = (, result) $ case (waiting, isReady) of
  (False, _) -> state { step = Print printState { waiting = True } }
  (True, False) -> state
  (True, True) -> let iNext = iPrintCol + 1
    in if iNext /= 0
      then state { step = Print printState
        { iPrintCol = iNext
        , waiting = False
        }
      }
      -- done with this row
      else if isWorkingOnNumerator state
        -- start work on the denominator
        then state
          { isWorkingOnNumerator = False
          , step = BCD $ initBCD valDenominator printValue
          }
        -- we're done!
        else state
          { isWorkingOnNumerator = True
          , step = Collect
          }
  where
    result :: OLEDResult
    result = defaultResult
      { showChar = boolToBit isReady
      , charValue = readyRow !! iPrintCol
      , rowIndex = if isWorkingOnNumerator state then 0 else 1
      , colIndex = bitCoerce (zeroExtend iPrintCol :: Index 16)
      }
