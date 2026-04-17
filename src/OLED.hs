module OLED where

import Clash.Prelude
import Calculator.Prelude
import Clash.Annotations.Primitive

{-# NOINLINE oledDriver #-}
{-# ANN oledDriver hasBlackBox #-}
oledDriver
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Signal dom Bit
  -> Signal dom Bit
  -> Signal dom (Unsigned 8)
  -> Signal dom (Unsigned 2)
  -> Signal dom (Unsigned 3)
  -> Signal dom (Unsigned 7)
oledDriver = undefined

oled
  :: HiddenClockResetEnable DomMain
  => Signal DomMain Bit  -- ^ Has data
  -> Signal DomMain CalcValue  -- ^ Data
  -> Signal DomMain (Unsigned 7)
oled bHasData bData = driver (pure 0) (pure 0) (pure 0) (pure 0) (pure 0)
  where
    driver = hideReset $ hideClock oledDriver
