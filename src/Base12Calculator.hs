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

accum sw btnL btnC btnR btnU btnD bRow = (bOledData, led, pure 0, bCol)
  where
    btnPush = debounce btnU
    btnPop = debounce btnD
    (bCol, bKeypad) = unbundle $ keypad bRow
    bDigits :: Signal DomMain (BitVector 12)
    (bDigits, bButtonsRow) = unbundle $ split . withVector reverse <$> bKeypad
    bDigit :: Signal DomMain (Maybe (Unsigned 4))
    bDigit = ifoldl (\ma i isUp -> if isUp
      then ma <|> Just (bitCoerce i)
      else ma) Nothing . unpack <$> bDigits
    bWIPValue = inlineMoore (CalcValue False 0 1) bDigit $ \value mDigit -> case mDigit of
      Nothing -> value
      Just digit -> value { valNumerator = mul12 (valNumerator value) + zeroExtend digit }
        where
          mul12 n = n .<<. 3 + n .<<. 2
    led = zeroExtend <$> delayMaybe 0 (fmap (+1) <$> bDigit)
    -- led = bDigit ## \case
    --   Nothing -> 0
    --   Just x -> zeroExtend (x + 1)
    bStateAction = (,,) <$> btnPush <*> btnPop <*> bWIPValue ## \case
      (1, 0, value) -> StackPush value
      (0, 1, _) -> StackPop
      _ -> StackAck
    (bTop, bStackResult) = unbundle $ stack bStateAction
    bOledData = oled $ Just <$> bWIPValue

counter = flip mealy 0 $ \cases
  n 1 -> (n + 1, n + 1)
  n 0 -> (n, n)

data StackAction = StackPush CalcValue | StackPop | StackAck
  deriving (Eq, Ord, Show, Generic)
data StackResult = StackIdle | StackYield CalcValue | StackOverUnderflow Bool
  deriving (Eq, Ord, Show, Generic, NFDataX)
data StackState = StackState
  { stackTop :: Unsigned 16
  , stackItems :: Vec 16 CalcValue
  } deriving (Eq, Ord, Show, Generic, NFDataX)
stackMachine :: StackState -> StackAction -> (StackState, (Unsigned 16, StackResult))
stackMachine state@(StackState top items) = let ok (transferred@(StackState top _), result) = (transferred, (top, result)) in ok . \case
  StackPush value -> if top == 16
    then (state, StackOverUnderflow True)
    else (StackState (top + 1) (replace top value items), StackIdle)
  StackPop -> if top == 0
    then (state, StackOverUnderflow False)
    else let i = top - 1 in (StackState i items, StackYield $ items !! i)
  StackAck -> (state, StackIdle)
stack = mealy stackMachine $ StackState 0 $ repeat calcValueZero
