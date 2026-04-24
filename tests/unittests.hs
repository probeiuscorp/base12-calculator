import Prelude

import Test.Tasty

import qualified Tests.Calculator.TestSnapshots

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Calculator.TestSnapshots.tests
  ]
