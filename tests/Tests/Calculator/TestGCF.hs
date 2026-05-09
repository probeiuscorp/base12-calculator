module Tests.Calculator.TestGCF where

import Prelude hiding (showChar)
import Calculator.Prelude
import qualified Clash.Prelude as C
import SnapshotTesting

import Test.Tasty
import GCF

showUint8Pair :: Maybe (C.Unsigned 8, C.Unsigned 8) -> String
showUint8Pair = \case
  Just (a', b') -> snapshotPrint a' <> "," <> snapshotPrint b'
  Nothing -> '-' <$ "0b0111_1000,0b0101_0000"

tests :: TestTree
tests = testGroup "gcf"
  [ simple (10, 20)
  , simple (32, 32)
  , simple (9, 21)
  , simple (21, 9)
  , simple (120, 80)
  ]
  where
    simple :: (C.Unsigned 8, C.Unsigned 8) -> TestTree
    simple (a, b) = snapshot @C.System "gcf" (show a <> "–" <> show b) 30 $
      let
        bStart = fromEdges False [(3, True), (4, False)]
        bmAB = bStart ## \start -> if start then Just (a, b) else Nothing
        bmData = gcf'er bmAB
      in
        [ raw "a,b" $ showUint8Pair <$> bmAB
        , raw "gcf" $ showMaybe <$> bmData
        ]
