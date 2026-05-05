-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Base12Calculator where

import Clash.Prelude
import Calculator.Prelude
import OLED (oled)
import Keypad (keypad)
import Clash.Sized.Vector.ToTuple (VecToTuple(vecToTuple))

topEntity
  :: Clock DomMain
  -> Reset DomMain
  -> Enable DomMain
  -> Signal DomMain Bit
  -> Signal DomMain Bit
  -> Signal DomMain Bit
  -> Signal DomMain Bit
  -> Signal DomMain Bit
  -> Signal DomMain Bit
  -> Signal DomMain (BitVector 4)
  -> (Signal DomMain (Unsigned 7)
    , Signal DomMain (Unsigned 16)
    , Signal DomMain (Unsigned 4)
    , Signal DomMain (BitVector 4)
    )
topEntity = exposeClockResetEnable accum

-- To specify the names of the ports of our top entity, we create a @Synthesize@ annotation.
{-# ANN topEntity
  (Synthesize
    { t_name = "top"
    , t_inputs =
      [ PortName "clk"
      , PortName "rst"
      , PortName "en"
      , PortName "sw"
      , PortName "btnL"
      , PortName "btnC"
      , PortName "btnR"
      , PortName "btnU"
      , PortName "btnD"
      , PortName "row"
      ]
    , t_output = PortProduct ""
      [ PortName "oled"
      , PortName "led"
      , PortName "an"
      , PortName "col"
      ]
    }) #-}

-- Make sure GHC does not apply any optimizations to the boundaries of the design.
-- For GHC versions 9.2 or older, use: {-# NOINLINE topEntity #-}
{-# OPAQUE topEntity #-}

data WorkingValueAction
  = WIPDigit (Unsigned 4) | WIPPop CalcValue
  | WIPReset | WIPPush | WIPNegate | WIPReciprocal
  deriving (Eq, Ord, Show, Generic, NFDataX)
accum sw btnL btnC btnR btnU btnD bRow = (bOledData, led, pure 0, bCol)
  where
    btnPush = boolToBit <$> bPush
    btnPop = bitToBool <$> debounce btnD
    (bCol, bKeypad) = unbundle $ keypad bRow
    bDigits :: Signal DomMain (BitVector 12)
    (bDigits, bButtonsRow) = unbundle $ split . withVector reverse <$> bKeypad
    bDigit :: Signal DomMain (Maybe (Unsigned 4))
    bDigit = ifoldl (\ma i isUp -> if isUp
      then ma <|> Just (bitCoerce i)
      else ma) Nothing . unpack <$> bDigits

    bBackspace, bPush, bNegate, bReciprocal :: Signal DomMain Bool
    (bBackspace, bPush, bNegate, bReciprocal) = vecToTuple $ traverse bitCoerce bButtonsRow
    bWIPAction = WIPDigit $$$: bDigit
      <||> ((,) <$> btnPop <*> bmTopOfStack ## \(isPop, mValue) -> if isPop
        then Just $ WIPPop $ mValue ?? defaultWorkingValue
        else Nothing)
      <||> (mWhen WIPPush <$> bPush)
      <||> (mWhen WIPNegate <$> bNegate)
      <||> (mWhen WIPReciprocal <$> bReciprocal)
      <||> (mWhen WIPReset <$> bBackspace)
    defaultWorkingValue = CalcValue False 0 1
    bWIPValue = inlineMoore defaultWorkingValue bWIPAction $ withNoop $ \value -> \case
      WIPPush -> CalcValue False 0 1
      WIPPop fromStack -> fromStack
      WIPNegate -> value { valIsNegative = not $ valIsNegative value }
      WIPReciprocal -> value
        { valNumerator = valDenominator value
        , valDenominator = valNumerator value
        }
      WIPReset -> value { valNumerator = 0 }
      WIPDigit digit -> value { valNumerator = mul12 (valNumerator value) + zeroExtend digit }
        where
          mul12 n = n .<<. 3 + n .<<. 2

    led = zeroExtend <$> bStackSize
    bStateAction = (,,) <$> btnPush <*> btnPop <*> bWIPValue ## \case
      (1, False, value) -> StackPush value
      (0, True, _) -> StackPop
      _ -> StackAck
    (bmTopOfStack, bStackSize, bStackResult) = unbundle $ stack bStateAction
    bOledData = oled $ Just <$> bWIPValue

counter = flip mealy 0 $ \cases
  n 1 -> (n + 1, n + 1)
  n 0 -> (n, n)

data StackAction = StackPush CalcValue | StackPop | StackAck
  deriving (Eq, Ord, Show, Generic, NFDataX)
data StackResult = StackIdle | StackYield CalcValue | StackOverUnderflow Bool
  deriving (Eq, Ord, Show, Generic, NFDataX)
data StackState = StackState
  { stackTop :: Unsigned 4
  , stackItems :: Vec 16 CalcValue
  } deriving (Eq, Ord, Show, Generic, NFDataX)
stackMachine :: StackState -> StackAction -> (StackState, (Maybe CalcValue, Unsigned 4, StackResult))
stackMachine state@(StackState top items) = let
    ok (transferred@(StackState top' items'), result) = (transferred, (head items' `mWhen` (top' > 0), top', result))
  in ok . \case
    StackPush value -> if top == 16
      then (state, StackOverUnderflow True)
      else (StackState (top + 1) (replace top value items), StackIdle)
    StackPop -> if top == 0
      then (state, StackOverUnderflow False)
      else let i = top - 1 in (StackState i items, StackYield $ items !! i)
    StackAck -> (state, StackIdle)
stack = mealy stackMachine $ StackState 0 $ repeat calcValueZero
