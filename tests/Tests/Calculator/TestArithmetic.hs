module Tests.Calculator.TestArithmetic where

import Prelude hiding (showChar)
import Calculator.Prelude
import qualified Clash.Prelude as C
import SnapshotTesting

import Test.Tasty
import Tests.Calculator.TestGCF (showUint8Pair)
import Arithmetic

tests :: TestTree
tests = testGroup "flooredDivide"
  [ simple (20, 3)
  , simple (32, 32)
  , simple (32, 34)
  , simple (21, 9)
  , simple (128, 4)
  , simple (128, 5)
  , simple (120, 80)
  , simple (120, 7)
  ]
  where
    simple :: (C.Unsigned 8, C.Unsigned 8) -> TestTree
    simple ratio@(a, b) = snapshot @C.System "flooredDivide" (show a <> "÷" <> show b) 30 $
      let
        bRatio = fromEdges Nothing [(6, Just ratio), (7, Nothing)]
        bQuotient = flooredDivide @8 bRatio
      in
        [ raw "ratio" $ showUint8Pair <$> bRatio
        , raw "quotient" $ showMaybe <$> bQuotient
        ]
