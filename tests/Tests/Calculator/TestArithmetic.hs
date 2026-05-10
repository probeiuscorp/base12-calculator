module Tests.Calculator.TestArithmetic (tests) where

import Prelude hiding (showChar)
import Calculator.Prelude
import qualified Clash.Prelude as C
import SnapshotTesting

import Test.Tasty
import Tests.Calculator.TestGCF (showUint8Pair)
import Arithmetic

tests :: TestTree
tests = testGroup "arithmetic"
  [ divideCases
  , simplifyCases
  , arithmeticCases
  ]
  where
    divideCase ratio@(a, b) = snapshot @C.System "flooredDivide" (show a <> "÷" <> show b) 30 $
      let
        bRatio = fromEdges Nothing [(6, Just ratio), (7, Nothing)]
        bQuotient = (flooredDivide @8) bRatio
      in
        [ raw "ratio" $ showUint8Pair <$> bRatio
        , raw "quotient" $ showMaybe <$> bQuotient
        ]
    divideCases = testGroup "flooredDivide"
      [ divideCase (1, 1)
      , divideCase (32, 32)
      , divideCase (20, 3)
      , divideCase (32, 34)
      , divideCase (21, 9)
      , divideCase (128, 4)
      , divideCase (128, 5)
      , divideCase (120, 80)
      , divideCase (120, 7)
      ]
    simplifyCase ratio@CalcValue{..} = snapshot @C.System "simplify" (show valNumerator <> "÷" <> show valDenominator) 120 $
      let
        bRatio = fromEdges Nothing [(6, Just ratio), (7, Nothing)]
        bSimplified = simplify bRatio
      in
        [ raw "ratio" $ printMaybeCalcValue <$> bRatio
        , raw "quotient" $ printMaybeCalcValue <$> bSimplified
        ]
    simplifyCases = testGroup "simplify"
      [ simplifyCase $ CalcValue False 1 1
      , simplifyCase $ CalcValue True 4 2
      , simplifyCase $ CalcValue True 20 5
      , simplifyCase $ CalcValue False 18 12
      , simplifyCase $ CalcValue False 200 18
      , simplifyCase $ CalcValue False 200 200
      , simplifyCase $ CalcValue False 8 30
      , simplifyCase $ CalcValue True 30 40
      ]
    prettyPrintOp = \case
      ArithSum -> "+"
      ArithSub -> "−"
      ArithMul -> "×"
      ArithDiv -> "÷"
    arithmeticCase op a b = snapshot @C.System "arithmetic" (unwords [show a, prettyPrintOp op, show b]) 120 $
      let
        bmAction = fromEdges Nothing [(6, Just op), (7, Nothing)]
        (bmError, bmResult) = arithmetic (pure $ Just a) (pure $ Just b) bmAction
      in
        [ raw "action" $ bmAction ## maybe ('-' <$ show ArithSum) show
        , col "err" bmError
        , raw "result" $ printMaybeCalcValue <$> bmResult
        ]
    arithmeticCases = testGroup "arithmetic"
      [ arithmeticCase ArithSum (CalcValue False 10 20) (CalcValue False 10 20)
      , arithmeticCase ArithSum (CalcValue False 10 20) (CalcValue True 10 20)
      , arithmeticCase ArithSub (CalcValue False 10 20) (CalcValue False 10 20)
      , arithmeticCase ArithSub (CalcValue False 10 20) (CalcValue True 10 20)
      , arithmeticCase ArithMul (CalcValue False 5 3) (CalcValue True 4 3)
      , arithmeticCase ArithDiv (CalcValue False 2 8) (CalcValue True 4 2)
      ]
