{-# LANGUAGE MultiWayIf #-}

module GCF (gcf'er) where

import Clash.Prelude
import Calculator.Prelude

data GCFStepState n = GCFStepState
  { a :: Unsigned n
  , b :: Unsigned n
  , nSharedPowers :: Unsigned 6
  } deriving (Eq, Ord, Show, Generic, NFDataX)
data GCFState n
  = Wait
  | FactorOut2's (GCFStepState n)
  | FactorOutOthers (GCFStepState n)
  deriving (Eq, Ord, Show, Generic, NFDataX)

-- | Binary Euclidean GCF algorithm
gcf'er
  :: (KnownNat n, KnownDomain dom, HiddenClockResetEnable dom)
  => Signal dom (Maybe (Unsigned n, Unsigned n))
  -> Signal dom (Maybe (Unsigned n))
gcf'er = mealy transfer Wait
  where
    ok = (, Nothing)
    transfer s mAB = case s of
      Wait -> case mAB of
        Just (a, b) -> if a == 0 || b == 0
          -- special case: if numerator or denominator are zero, just answer with 1
          then (Wait,) . Just $ if a == 0 && b == 0
            then 1
            else max a b
          else ok $ FactorOut2's $ GCFStepState a b 0
        Nothing -> ok s
      FactorOut2's stepState@GCFStepState{..} -> ok $
        if bitToBool (lsb a) || bitToBool (lsb b)
          then FactorOutOthers stepState
          else FactorOut2's GCFStepState
            { a = a .>>. 1
            , b = b .>>. 1
            , nSharedPowers = nSharedPowers + 1
            }
      FactorOutOthers stepState@GCFStepState{..} -> if a /= b
        then ok $ FactorOutOthers $ if
          -- eliminate remaining extraneous factors of two
          | not (bitToBool $ lsb a) -> stepState { a = a .>>. 1 }
          | not (bitToBool $ lsb b) -> stepState { b = b .>>. 1 }

          | a > b -> stepState { a = (a - b) .>>. 1 }
          | otherwise -> stepState { b = (b - a) .>>. 1 }
        -- TODO: Find a way to not use fromIntegral
        else let mGCF = Just (a .<<. fromIntegral nSharedPowers) in
          (Wait, mGCF)
