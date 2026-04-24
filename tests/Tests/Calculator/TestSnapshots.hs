module Tests.Calculator.TestSnapshots where

import Prelude
import qualified Clash.Prelude as C
import SnapshotTesting

import Test.Tasty

tests :: TestTree
tests = snapshot "testing setup" "snapshots" 10
  [ col "ones" (pure 1 :: C.Signal C.System (C.Unsigned 8))
  , col "twos" (pure 2 :: C.Signal C.System (C.Unsigned 6))
  , col "threes" (pure 3 :: C.Signal C.System (C.Unsigned 4))
  ]
