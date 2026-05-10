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
  => Signal dom (Maybe CalcValue)
  -> Signal dom (Maybe CalcValue)
  -> Signal dom (Maybe ArithmeticAction)
  -> (Signal dom Bool, Signal dom (Maybe CalcValue))  -- ^ raised to True when not enough values
arithmetic bma bmb bmAction = (bError, simplify bmOutput)
  where
    asSigned :: CalcValue -> (Signed NCalcValueBits, Signed NCalcValueBits)
    asSigned CalcValue{..} = ((if valIsNegative then negate else id) $ bitCoerce valNumerator, bitCoerce valDenominator)
    toSignMagnitude :: Signed NCalcValueBits -> (Bool, Unsigned NCalcValueBits)
    toSignMagnitude v = if v < 0
      then (True, bitCoerce (-v))
      else (False, bitCoerce v)
    asUnsigned :: (Signed NCalcValueBits, Signed NCalcValueBits) -> CalcValue
    asUnsigned (toSignMagnitude -> (nSign, numerator), toSignMagnitude -> (dSign, denominator)) =
      CalcValue (nSign /= dSign) numerator denominator

    (bError, bmOutput) = unbundle $ bundle (bmAction, bma, bmb) ## \case
      (Nothing, _, _) -> (False, Nothing)
      (Just act, Just (asSigned -> (an, ad)), Just (asSigned -> (bn, bd))) -> let
        value = asUnsigned $ case act of
          ArithSum -> (an * bd + bn * ad, ad * bd)
          ArithSub -> (an * bd - bn * ad, ad * bd)
          ArithMul -> (an * bn, ad * bd)
          ArithDiv -> (an * bd, bn * ad)
        in (False, Just value)
      (Just _, _, _) -> (True, Nothing)


-- || Start of notable snippet!!
-- Here `simplify` and `simplifyHandshake` are (as far as I can tell) exactly the same!
-- Just simplifyHandshake is much simpler, less error-prone, shorter, and more divisible + composable

data SimplifyState
  = SimplifyInit
  | SimplifyGCF CalcValue
  | SimplifyDivide CalcValue CalcTerm (Maybe CalcTerm)
  deriving (Eq, Ord, Show, Generic, NFDataX)
-- | Takes a fraction that may be unsimplified like 16/10 and simplify it (such as into 8/5)
simplifyMealy
  :: HiddenClockResetEnable dom
  => Signal dom (Maybe CalcValue)
  -> (Signal dom (Maybe CalcValue), Signal dom (Maybe CalcTerm))
simplifyMealy bmValue = (bmSimplified, bmQuotient)
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

-- alias a good old Haskell function for familiarity
-- this is the pipeline operator!!
(|>) = (&)
infixl 1 |>

simplify
  :: (KnownDomain dom, HiddenClockResetEnable dom)
  -- HS.Handshake dom a b is an alias for
  -- (Signal dom (Maybe a)) -> (Signal dom (Maybe b))
  => HS.Handshake dom CalcValue CalcValue
simplify =
  -- first the gcf'er (one whole valid identifier) expects two numbers, not a whole CalcValue
  HS.mapin (\CalcValue{..} -> (valNumerator, valDenominator))
  -- we can now apply this input mapping to the gcf'er Handshake
  |> ($ gcf'er)
  -- recall the CalcValue we gave to gcf'er, as that's what we want to use the gcf against!
  -- we now have prepared two sets of ratios for flooredDivide to compute
  |> HS.hold (\CalcValue{..} gcf -> ((valNumerator, gcf), (valDenominator, gcf)))
  -- We don't want to instantiate two flooredDividers to parallelize this work,
  -- instead we just want to reuse the same instantation to compute one set then the other.
  -- Take the Handshake we've so far built and compose it with this reconfigured flooredDivide
  |> (>>> HS.reuse flooredDivide)
  -- Now we just need to recall the negative sign from our overall input,
  -- and we can yield our simplified CalcValue!!
  |> HS.holdSome valIsNegative (\isNegative (numerator, denominator) -> CalcValue isNegative numerator denominator)

-- || End of notable snippet...


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
