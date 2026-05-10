module Calculator.Handshake
  ( Handshake
  , mapin, mapout, holdSome, hold, reuse
  ) where

import Clash.Prelude
import Calculator.Prelude

type Handshake dom a b = Signal dom (Maybe a) -> Signal dom (Maybe b)

mapin
  :: (a -> b)
  -> Handshake dom b o
  -> Handshake dom a o
mapin f hs bma = hs $ f $$$: bma

mapout
  :: (a -> b)
  -> Handshake dom i a
  -> Handshake dom i b
mapout f hs bma = f $$$: hs bma

holdSome
  :: (KnownDomain dom, HiddenClockResetEnable dom, NFDataX i, NFDataX i', NFDataX a, NFDataX b)
  => (i -> i')
  -> (i' -> a -> b)
  -> Handshake dom i a
  -> Handshake dom i b
holdSome takeSlice f hs bmi = bmb
  where
    bma = hs bmi
    bmb = inlineMealyB Nothing (bmi, bma) $ \s (mi, ma) -> case s of
      Nothing -> case mi of
        Nothing -> (s, Nothing)
        Just i -> (Just (takeSlice i), Nothing)
      Just i' -> case ma of
        Nothing -> (s, Nothing)
        Just a -> (Nothing, Just (f i' a))
-- | Specialization to hold all of input
hold = holdSome id

data ReuseState a b
  = ReuseWait
  | ReuseReceived a
  | ReuseOneDone b
  deriving (Eq, Ord, Show, Generic, NFDataX)
reuse
  :: (KnownDomain dom, HiddenClockResetEnable dom, NFDataX a, NFDataX b)
  => Handshake dom a b
  -> Handshake dom (a, a) (b, b)
reuse hs bmaa = bmbb
  where
    bmb = hs bma
    idle = (, (Nothing, Nothing))
    (bma, bmbb) = inlineMealyB ReuseWait (bmaa, bmb) $ \s (maa, mb) -> case s of
      ReuseWait -> case maa of
        Nothing -> idle s
        Just (a1, a2) -> (ReuseReceived a2, (Just a1, Nothing))
      ReuseReceived a2 -> case mb of
        Nothing -> idle s
        Just b1 -> (ReuseOneDone b1, (Just a2, Nothing))
      ReuseOneDone b1 -> case mb of
        Nothing -> idle s
        Just b2 -> (ReuseWait, (Nothing, Just (b1, b2)))
