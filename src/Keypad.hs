module Keypad where

import Clash.Prelude
import Clash.Annotations.Primitive
import Calculator.Prelude

createDomain vSystem{vName = "DomKeypad", vPeriod = hzToPeriod 50e3}

{-# OPAQUE keypadDriver #-}
{-# ANN keypadDriver hasBlackBox #-}
keypadDriver
  :: Clock dom
  -> Reset dom
  -> Signal dom (BitVector 4)
  -> Signal dom (BitVector 20)
keypadDriver !_ !_ !_ = pure 0

keypad
  :: forall dom. (KnownDomain dom, HiddenClockResetEnable dom)
  => Signal dom (BitVector 4)
  -> Signal dom (BitVector 4, BitVector 16)
keypad bRows = bundle (bCols, bitCoerce <$> debouncedKeys)
  where
    implicitDriver = hideClock keypadDriver (unsafeToReset $ pure True)
    bRawKeys :: Signal dom (Vec 16 Bit)
    (bCols, bRawKeys) = unbundle $ second bitCoerce . split <$> implicitDriver bRows
    debouncedKeys = debounce `traverse` sequenceA bRawKeys
