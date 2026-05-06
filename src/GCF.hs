{-# LANGUAGE MultiWayIf #-}

module GCF where

import Clash.Prelude
import Calculator.Prelude

data GCFStepState n = GCFStepState
  { a :: Unsigned n
  , b :: Unsigned n
  , nSharedPowers :: Unsigned 6
  } deriving (Eq, Ord, Show, Generic, NFDataX)
data GCFState n
  = Wait (Maybe (Unsigned n))
  | FactorOut2's (GCFStepState n)
  | FactorOutOthers (GCFStepState n)
  deriving (Eq, Ord, Show, Generic, NFDataX)

-- | Binary Euclidean GCF algorithm
gcf
  :: (KnownNat n, KnownDomain dom, HiddenClockResetEnable dom)
  => Signal dom (Unsigned n)
  -> Signal dom (Unsigned n)
  -> Signal dom Bool
  -> Signal dom (Maybe (Unsigned n))
gcf ba bb bStart = mealy transfer (Wait Nothing) $ bundle (ba, bb, bStart)
  where
    ok = (, Nothing)
    transfer s (a0, b0, start) = case s of
      Wait mHeldTerm -> if start
        then ok $ FactorOut2's $ GCFStepState a0 b0 0
        else (s, mHeldTerm)
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
          (Wait mGCF, mGCF)
