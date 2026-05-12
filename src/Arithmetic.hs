{-# LANGUAGE BlockArguments #-}

module Arithmetic (arithmetic, ArithmeticAction(..), simplify, flooredDivide) where

import Clash.Prelude
import Calculator.Prelude
import GCF
import Control.Arrow ((>>>))
import qualified Calculator.Handshake as HS

data ArithmeticAction = ArithSum | ArithSub | ArithMul | ArithDiv
  deriving (Eq, Ord, Show, Generic, NFDataX)
data ArithmeticState
  = InitWait
  | GCFWait ArithmeticAction
  deriving (Eq, Ord, Show, Generic, NFDataX)

arithmetic
  :: HiddenClockResetEnable dom
  => HS.Handshake dom (ArithmeticAction, CalcValue, CalcValue) CalcValue
arithmetic = fmap (fmap transfer) >>> simplify
  where
    transfer (act, asSigned -> (an, ad), asSigned -> (bn, bd)) =
      asUnsigned $ case act of
        ArithSum -> (an * bd + bn * ad, ad * bd)
        ArithSub -> (an * bd - bn * ad, ad * bd)
        ArithMul -> (an * bn, ad * bd)
        ArithDiv -> (an * bd, bn * ad)

    asSigned :: CalcValue -> (Signed NCalcValueBits, Signed NCalcValueBits)
    asSigned CalcValue{..} = ((if valIsNegative then negate else id) $ bitCoerce valNumerator, bitCoerce valDenominator)
    toSignMagnitude :: Signed NCalcValueBits -> (Bool, Unsigned NCalcValueBits)
    toSignMagnitude v = if v < 0
      then (True, bitCoerce (-v))
      else (False, bitCoerce v)
    asUnsigned :: (Signed NCalcValueBits, Signed NCalcValueBits) -> CalcValue
    asUnsigned (toSignMagnitude -> (nSign, numerator), toSignMagnitude -> (dSign, denominator)) =
      CalcValue (nSign /= dSign) numerator denominator

simplify
  :: (KnownDomain dom, HiddenClockResetEnable dom)
  => HS.Handshake dom CalcValue CalcValue
simplify =
  HS.mapin (\CalcValue{..} -> (valNumerator, valDenominator))
  & ($ gcf'er)
  & HS.hold (\CalcValue{..} gcf -> ((valNumerator, gcf), (valDenominator, gcf)))
  & (>>> HS.reuse flooredDivide)
  & HS.holdSome valIsNegative (\isNegative (numerator, denominator) -> CalcValue isNegative numerator denominator)

-- Adapted from https://gist.github.com/vvaltchev/d9f680b70a372559f88105daae846d8e
flooredDivide
  :: forall n dom. (KnownNat n, HiddenClockResetEnable dom, 1 <= n)
  => Signal dom (Maybe (Unsigned n, Unsigned n))  -- ^ (numerator, denominator)
  -> Signal dom (Maybe (Unsigned n))
flooredDivide bmAB = bmQuotient
  where
    remainingSteps :: Index n
    remainingSteps = fromSNat (SNat :: SNat  (n - 1))
    defaultState = Nothing :: Maybe
      ( Unsigned n  -- ^ held numerator
      , Unsigned n  -- ^ held denominator
      , Unsigned n  -- ^ working quotient
      , Index n  -- ^ step
      , Unsigned n  -- ^ remainder
      )
    bmQuotient = inlineMealy defaultState bmAB $ \s mAB -> case s of
      Nothing -> case mAB of
        Nothing -> (Nothing, Nothing)
        Just (a, b) -> (Just (a, b, 0, remainingSteps, 0), Nothing)
      Just (n, d, q, i, r) -> let
        iIndexable :: Int
        iIndexable = fromIntegral i
        r' = bitCoerce $ bitCoerce r .<<+ lsb (n .>>. iIndexable)
        (r'', q') = if r' >= d
          then (r' - d, q .|. 1 .<<. iIndexable)
          else (r', q)
        in if i /= 0
          then (Just (n, d, q', i - 1, r''), Nothing)
          else (Nothing, Just q')
