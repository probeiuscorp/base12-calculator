module Calculator.Handshake
  ( Handshake
  , mapin, mapout, dimap
  , Calculator.Handshake.first, holdSome, hold, reuse
  ) where

import Clash.Prelude
import Calculator.Prelude hiding (first)

-- | A Handshake is an alias / useful way of reasoning about multi-clock-cycle operations.
-- In Verilog I called these "four signal handshakes." One module would have a "input" and
-- "ready" signal while the other would have an "output" and "done" signal.
-- Despite how common this is it's really painful in Verilog, with so much room to make mistakes.
-- With Handshake's however, composing two such operations is actually just regular function composition!

-- The expectation is that when the consumer (whoever owns the "input") is "ready",
-- they will switch the `Maybe a` to a Just for one clock cycle with the valid input.
-- Then when the producer is "done", they'll switch the `Maybe b` to a Just for one clock
-- cycle with the valid output.
type Handshake dom a b = Signal dom (Maybe a) -> Signal dom (Maybe b)

-- | Apply simple transformation to the input a Handshake is expecting
mapin
  :: (a -> b)  -- ^ the simple transformation
  -> Handshake dom b o  -- ^ original Handshake
  -> Handshake dom a o  -- ^ your new Handshake
mapin f hs bma = hs $ f $$$: bma

-- | Apply simple transformation to the output a Handshake gives
mapout
  :: (a -> b)  -- ^ the simple transformation
  -> Handshake dom i a  -- ^ original Handshake
  -> Handshake dom i b  -- ^ your new Handshake
mapout f hs bma = f $$$: hs bma

-- | Apply both a mapin and a mapout
dimap
  :: (i' -> i)
  -> (o -> o')
  -> Handshake dom i o
  -> Handshake dom i' o'
dimap lmap rmap = mapin lmap . mapout rmap

-- | Handshakes distribute over products
first
  :: (KnownDomain dom, HiddenClockResetEnable dom, NFDataX i, NFDataX o, NFDataX a)
  => Handshake dom i o
  -> Handshake dom (i, a) (o, a)
first hs bmia = bmoa
  where
    bmo = hs $ delay Nothing bmi
    (bmi, bmoa) = inlineMealyB Nothing (bmia, bmo) $ \s (mia, mo) -> case s of
      Nothing -> case mia of
        Nothing -> (Nothing, (Nothing, Nothing))
        Just (i, a) -> (Just a, (Just i, Nothing))
      Just a -> case mo of
        Nothing -> (s, (Nothing, Nothing))
        Just o -> (Nothing, (Nothing, Just (o, a)))

-- | Hold/recall the original input when given Handshake is done.
-- Mappings and composition are the two fundamental means of using Handshakes, however
-- these are both **linear** (affine) operations. Each input is forgotten by the end!
-- With `hold` you can compute another Handshake then combine that result into the 'current state'.
hold
  :: (KnownDomain dom, HiddenClockResetEnable dom, NFDataX i, NFDataX a, NFDataX b)
  => (i -> a -> b)  -- ^ combine the initial input with the Handshake's result
  -> Handshake dom i a  -- ^ original Handshake that turns i's into a's in time
  -> Handshake dom i b
hold = holdSome id

-- | Generalization of `hold` that permits taking a slice of the 'current state'.
-- This is only useful as an optimization: instead of holding 128 bits when only 4 are useful,
-- you can specify to just hold those 4 bits.
holdSome
  :: (KnownDomain dom, HiddenClockResetEnable dom, NFDataX i, NFDataX i', NFDataX a, NFDataX b)
  => (i -> i')  -- ^ take a (smaller) slice of what you _need_ from the initial input
  -> (i' -> a -> b)  -- ^ combine a slice of the initial input with the Handshake's result
  -> Handshake dom i a  -- ^ original Handshake that turns i's into a's in time
  -> Handshake dom i b
holdSome takeSlice f hs bmi = bmb
  where
    bma = hs bmi
    bmb = inlineMealyB Nothing (bmi, bma) $ \s (mi, ma) -> case s of
      Nothing -> case (mi, ma) of
        (Nothing, _) -> (s, Nothing)
        (Just i, Just a) -> (s, Just $ f (takeSlice i) a)
        (Just i, _) -> (Just (takeSlice i), Nothing)
      Just i' -> case ma of
        Nothing -> (s, Nothing)
        Just a -> (Nothing, Just (f i' a))

data ReuseState a b
  = ReuseWait
  | ReuseReceived a
  | ReuseOneDone b
  deriving (Eq, Ord, Show, Generic, NFDataX)
-- | Given a Handshake from a to b, this will create a new Handshake that takes two a's
-- and will pass them, serially, through only one instantiation of the given Handshake.
reuse
  :: (KnownDomain dom, HiddenClockResetEnable dom, NFDataX a, NFDataX b)
  => Handshake dom a b  -- ^ Handshake that does one thing at a time
  -> Handshake dom (a, a) (b, b)  -- ^ your new Handshake that'll be ran twice on one instantiation of your original Handshake
reuse hs bmaa = bmbb
  where
    bmb = hs $ delay Nothing bma
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
