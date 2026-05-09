module Arithmetic (arithmetic, simplify, flooredDivide) where

import Clash.Prelude
import Calculator.Prelude
import GCF

data ArithmeticAction = ArithSum | ArithSub | ArithMul | ArithDiv
  deriving (Eq, Ord, Show, Generic, NFDataX)
data ArithmeticState
  = InitWait
  | GCFWait ArithmeticAction
  deriving (Eq, Ord, Show, Generic, NFDataX)

arithmetic
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe CalcValue)
  -> Signal dom (Maybe CalcValue)
  -> Signal dom (Maybe ArithmeticAction)
  -> (Signal dom Bool, Signal dom (Maybe CalcValue))  -- ^ raised to True when not enough values
arithmetic bma bmb bmAction = (bError, bmOutput)
  where
    bmGCF = gcf'er bmGCFab
    (bError, bmOutput, bmGCFab) = mealyB transfer InitWait (bma, bmb, bmAction, bmGCF)

    ok = (, (False, Nothing, Nothing))
    transfer
      :: ArithmeticState
      -> (Maybe CalcValue, Maybe CalcValue, Maybe ArithmeticAction, Maybe CalcTerm)
      -> (ArithmeticState, (Bool, Maybe CalcValue, Maybe (CalcTerm, CalcTerm)))
    transfer s (ma, mb, mAction, mGCF) = case s of
      InitWait -> case mAction of
        Nothing -> undefined
        Just action -> (InitWait, undefined)
      GCFWait action -> case mGCF of
        Nothing -> ok s
        Just gcf -> undefined

data SimplifyState
  = SimplifyInit
  | SimplifyGCF CalcValue
  | SimplifyDivide CalcValue CalcTerm (Maybe CalcTerm)
  deriving (Eq, Ord, Show, Generic, NFDataX)
simplify
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe CalcValue)
  -> Signal dom (Maybe CalcValue)
simplify bmValue = bmSimplified
  where
    bmGCF = gcf'er bmGCFab
    bmQuotient = flooredDivide bmRatio
    ok = (, (Nothing, Nothing, Nothing))
    (bmGCFab, bmRatio, bmSimplified) = inlineMealyB SimplifyInit (bmValue, bmGCF, bmQuotient) $ \s (mValue, mGCF, mQuotient) -> case s of
      SimplifyInit -> case mValue of
        Nothing -> ok s
        Just value ->
          (SimplifyGCF value, (Just (valNumerator value, valDenominator value), Nothing, Nothing))
      SimplifyGCF value -> case mGCF of
        Nothing -> ok s
        Just gcf ->
          (SimplifyDivide value gcf Nothing, (Nothing, Just (valNumerator value, gcf), Nothing))
      SimplifyDivide value gcf mNumerator -> case mQuotient of
        Nothing -> ok s
        Just received -> case mNumerator of
          Nothing -> (SimplifyDivide value gcf (Just received), (Nothing, Just (valDenominator value, gcf), Nothing))
          Just numerator ->
            (SimplifyInit, (Nothing, Nothing, Just $ CalcValue (valIsNegative value) numerator received))

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
