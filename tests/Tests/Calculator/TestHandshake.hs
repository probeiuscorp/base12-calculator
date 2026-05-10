module Tests.Calculator.TestHandshake (tests) where

import Calculator.Prelude
import Clash.Prelude
import SnapshotTesting

import Test.Tasty
import qualified Calculator.Handshake as HS

hsInc :: (KnownDomain dom, HiddenClockResetEnable dom) => HS.Handshake dom (Unsigned 8) (Unsigned 8)
hsInc bmi = inlineMealy Nothing bmi $ \s mi -> case s of
  Nothing -> case mi of
    Nothing -> (Nothing, Nothing)
    Just i -> (Just (1 :: Unsigned 8, i + 1), Nothing)
  Just (n, i) -> if n > 0
    then (Just (n - 1, i), Nothing)
    else (Nothing, Just i)

impulse :: (KnownDomain dom, HiddenClockResetEnable dom, NFDataX a) => Int -> a -> Signal dom (Maybe a)
impulse t a = fromEdges Nothing [(t, Just a), (t + 1, Nothing)]

testHandshake :: String -> Unsigned 8 -> HS.Handshake System (Unsigned 8) (Unsigned 8) -> TestTree
testHandshake name input hs = snapshot "handshake" name 30 $ let
    bmIn = impulse 3 input
    bmOut = hs bmIn
  in
    [ raw "in" $ showMaybe <$> bmIn
    , raw "out" $ showMaybe <$> bmOut
    ]

tests :: TestTree
tests = fixClockResetEnable $ testGroup "handshake"
  [ testHandshake "simple" 2 hsInc
  , testHandshake "composed" 2 $ hsInc . hsInc
  , testHandshake "hold" 2 $ HS.hold (+) hsInc
  , testHandshake "mixed" 2 $ hsInc . HS.hold (+) hsInc
  , snapshot @System "handshake" "reuse" 30 $ let
      bmIn = impulse 2 (2 :: Unsigned 8)
      hs = splitAndSum $ HS.reuse hsInc
    in
      [ raw "in" $ showMaybe <$> bmIn
      , raw "out" $ showMaybe <$> hs bmIn
      ]
  , testHandshake "mixed reuse" 2
    $ HS.dimap (\a -> (a, a)) (uncurry (+))
    $ HS.reuse (HS.hold (+) hsInc)
  ]
    where
      splitAndSum = HS.dimap (\a -> (a, a)) (uncurry (+))
