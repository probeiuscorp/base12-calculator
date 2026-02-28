-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Base12Calculator where

import Clash.Prelude

-- Create a domain with the frequency of your input clock. For this example we used
-- 50 MHz.
createDomain vSystem{vName="Dom50", vPeriod=hzToPeriod 50e6}

-- | @topEntity@ is Clash@s equivalent of @main@ in other programming languages.
-- Clash will look for it when compiling "Example.Project" and translate it to
-- HDL. While polymorphism can be used freely in Clash projects, a @topEntity@
-- must be monomorphic and must use non- recursive types. Or, to put it
-- hand-wavily, a @topEntity@ must be translatable to a static number of wires.
--
-- Top entities must be monomorphic, meaning we have to specify all type variables.
-- In this case, we are using the @Dom50@ domain, which we created with @createDomain@
-- and we are using 8-bit unsigned numbers.
topEntity ::
  Clock Dom50 ->
  Reset Dom50 ->
  Enable Dom50 ->
  Signal Dom50 Bit ->
  Signal Dom50 Bit ->
  Signal Dom50 Bit ->
  Signal Dom50 Bit ->
  Signal Dom50 Bit ->
  Signal Dom50 Bit ->
  Signal Dom50 (Unsigned 4) ->
  (Signal Dom50 (Unsigned 7),
  Signal Dom50 (Unsigned 16),
  Signal Dom50 (Unsigned 4),
  Signal Dom50 (Unsigned 4))
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

accum sw btnL btnC btnR btnU btnD row = (pure 0, led, pure 0, pure 0)
  where
    led = counter $ debounce btnU 

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
