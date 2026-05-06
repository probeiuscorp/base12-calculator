module Tests.Calculator.TestGCF where

import Prelude hiding (showChar)
import Calculator.Prelude
import qualified Clash.Prelude as C
import SnapshotTesting

import Test.Tasty
import GCF

tests :: TestTree
tests = testGroup "gcf"
  [ simple (10, 20)
  , simple (32, 32)
  , simple (9, 21)
  , simple (21, 9)
  , simple (120, 80)
  ]
  where
    simple (a, b) = snapshot @C.System "gcf" (show a <> "–" <> show b) 30 cols
      where
        ba, bb :: C.Signal C.System (C.Unsigned 8)
        ba = pure a
        bb = pure b
        bStart = fromEdges False [(3, True), (4, False)]
        mData = gcf ba bb bStart
        cols =
          [ col "A" ba
          , col "B" bb
          , col "start" bStart
          , raw "mData" $ showMaybe <$> mData
          ]
