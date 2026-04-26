module Tests.Calculator.TestOLED where

import Prelude hiding (showChar)
import Calculator.Prelude
import qualified Clash.Prelude as C
import SnapshotTesting

import Test.Tasty
import OLED

tests :: TestTree
tests = let test = snapshot @C.System "oled" in testGroup "oled"
  -- TODO: rename test to basic
  [ test "test" 128 $ oledCols $ pure $ Just $ CalcValue False 20 30
  , test "switching value" 256 $ oledCols $ fromEdges Nothing
    [ (10, Just $ CalcValue False 20 30)
    , (138, Just $ CalcValue False 24 155)
    ]
  ]
  where
    showStep :: OLEDStep -> String
    showStep Collect = "ready"
    showStep Clearing = "clear"
    showStep (BCD _) = "bcd"
    showStep (Print _) = "print"
    oledCols bValue = fanOutOLEDResult bOLEDResult $ \showChar clearDisplay charValue iy ix ->
      [ col "ready" bReady
      , raw "state" (showStep . step <$> bOLEDState)
      , col "show char" showChar
      , col "clear" clearDisplay
      , col "char" charValue
      , col "row" iy
      , col "col" ix
      ]
      where
        bReady = pure True
        (bOLEDState, bOLEDResult) = C.unbundle $ oledMachine bValue bReady
