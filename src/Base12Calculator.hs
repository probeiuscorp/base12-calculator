-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Base12Calculator where

import Clash.Prelude
import Calculator.Prelude
import OLED (oled)

topEntity ::
  Clock DomMain ->
  Reset DomMain ->
  Enable DomMain ->
  Signal DomMain Bit ->
  Signal DomMain Bit ->
  Signal DomMain Bit ->
  Signal DomMain Bit ->
  Signal DomMain Bit ->
  Signal DomMain Bit ->
  Signal DomMain (Unsigned 4) ->
  (Signal DomMain (Unsigned 7),
  Signal DomMain (Unsigned 16),
  Signal DomMain (Unsigned 4),
  Signal DomMain (Unsigned 4))
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

accum sw btnL btnC btnR btnU btnD row = (bOledData, led, pure 0, pure 0)
  where
    btnPush = debounce btnU
    btnPop = debounce btnD
    bWIPValue = pure calcValueZero
    led = bTop
    bStateAction = (,,) <$> btnPush <*> btnPop <*> bWIPValue ## \case
      (1, 0, value) -> StackPush value
      (0, 1, _) -> StackPop
      _ -> StackAck
    (bTop, bStackResult) = unbundle $ stack bStateAction
    bOledData = oled $ pure $ Just $ CalcValue False 20 30

counter = flip mealy 0 $ \cases
  n 1 -> (n + 1, n + 1)
  n 0 -> (n, n)

data DebounceState = Wait | Pressing (Unsigned 8) | Releasing (Unsigned 8)
  deriving (Eq, Ord, Show, Generic, NFDataX)
debounceMachine :: DebounceState -> Bit -> (DebounceState, Bit)
debounceMachine = let ok = (, 0); alarmTime = 128 in \cases
  Wait 0 -> ok Wait
  Wait 1 -> ok $ Pressing alarmTime
  (Pressing _) 0 -> ok Wait
  (Pressing n) 1 -> if n == 0
    then (Releasing alarmTime, 1)
    else ok $ Pressing $ n - 1
  s@(Releasing _) 1 -> ok s
  (Releasing n) 0 -> ok $ if n == 0
    then Wait
    else Releasing $ n - 1
debounce = mealy debounceMachine Wait

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
