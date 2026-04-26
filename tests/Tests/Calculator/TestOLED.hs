module Tests.Calculator.TestOLED where

import Prelude hiding (showChar)
import Calculator.Prelude
import qualified Clash.Prelude as C
import SnapshotTesting

import Test.Tasty
import OLED

tests :: TestTree
tests = snapshot "oled" "test" 128 cols
  where
    bReady = pure True
    (bOLEDState, bOLEDResult) = C.unbundle $ oledMachine @C.System (pure $ Just $ CalcValue False 20 30) bReady
    showStep :: OLEDStep -> String
    showStep Collect = "ready"
    showStep Clearing = "clear"
    showStep (BCD _) = "bcd"
    showStep (Print _) = "print"
    cols = fanOutOLEDResult bOLEDResult $ \showChar clearDisplay charValue iy ix ->
      [ col "ready" bReady
      , raw "state" (showStep . step <$> bOLEDState)
      , col "show char" showChar
      , col "clear" clearDisplay
      , col "char" charValue
      , col "row" iy
      , col "col" ix
      ]
