import Prelude

import Test.Tasty

import qualified Tests.Calculator.TestSnapshots
import qualified Tests.Calculator.TestGCF
import qualified Tests.Calculator.TestOLED

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Calculator.TestSnapshots.tests
  , Tests.Calculator.TestGCF.tests
  , Tests.Calculator.TestOLED.tests
  ]
