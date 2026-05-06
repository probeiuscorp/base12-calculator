module Calculator.Prelude (
  module Calculator.Prelude,
  first, second, bimap,
) where

import Clash.Prelude
import Data.Bifunctor (first, second, bimap)
import Data.Maybe (fromMaybe)

type NCalcValueBits = 32
-- ceil (CalcValueBits / 3), add 2 to accommodate that Div here rounds down not up
type NBCDDigits = Div (NCalcValueBits + 3 - 1) 3
type NBCDBits = NBCDDigits * 4
type CalcTerm = Unsigned NCalcValueBits
data CalcValue = CalcValue
  { valIsNegative :: Bool
  , valNumerator :: CalcTerm
  , valDenominator :: CalcTerm
  } deriving (Eq, Ord, Show, Generic, NFDataX)
calcValueZero :: CalcValue
calcValueZero = CalcValue False 0 1

withPacking :: BitPack a => (BitVector (BitSize a) -> BitVector (BitSize a)) -> a -> a
withPacking f a = unpack $ f $ pack a
withUnPacking :: BitPack a => (a -> a) -> (BitVector (BitSize a) -> BitVector (BitSize a))
withUnPacking f a = pack $ f $ unpack a

withVector :: KnownNat n => (Vec n Bit -> Vec n Bit) -> BitVector n -> BitVector n
withVector f a = bitCoerce $ f $ bitCoerce a

createDomain vSystem{vName="DomMain", vResetPolarity=ActiveLow}

withGenClockResetEnable :: KnownDomain dom => (HiddenClockResetEnable dom => r) -> r
withGenClockResetEnable s = exposeClockResetEnable s clockGen resetGen enableGen

(##) :: Functor f => f a -> (a -> b)-> f b
fa ## f = f <$> fa
infixr 2 ##
($:) :: (a -> b) -> a -> b
($:) = ($)
infixr 6 $:
($$$:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
($$$:) = fmap . fmap
infixr 6 $$$:
(??) = flip fromMaybe

withNoop :: (s -> i -> s) -> (s -> Maybe i -> s)
withNoop transfer s = \case
  Just i -> transfer s i
  Nothing -> s
inlineMealy initialState bIn transfer = mealy transfer initialState bIn
inlineMoore initialState bIn transfer = moore transfer id initialState bIn
testTransfer
  :: (s -> i -> (s, o))
  -> (s -> i -> (s, (s, o)))
testTransfer transfer s i = let (s', o) = transfer s i in (s', (s, o))
testMealy
  :: (HiddenClockResetEnable dom, NFDataX s)
  => (s -> i -> (s, o))
  -> s
  -> Signal dom i
  -> Signal dom (s, o)
testMealy = mealy . testTransfer
unTestMealy
  :: Signal dom (s, o)
  -> Signal dom o
unTestMealy = fmap snd

data DebounceState = DebounceWait | DebouncePressing (Unsigned 8) | DebounceReleasing (Unsigned 8)
  deriving (Eq, Ord, Show, Generic, NFDataX)
debounceMachine :: DebounceState -> Bit -> (DebounceState, Bit)
debounceMachine = let ok = (, 0); alarmTime = 128 in \cases
  DebounceWait 0 -> ok DebounceWait
  DebounceWait 1 -> ok $ DebouncePressing alarmTime
  (DebouncePressing _) 0 -> ok DebounceWait
  (DebouncePressing n) 1 -> if n == 0
    then (DebounceReleasing alarmTime, 1)
    else ok $ DebouncePressing $ n - 1
  s@(DebounceReleasing _) 1 -> ok s
  (DebounceReleasing n) 0 -> ok $ if n == 0
    then DebounceWait
    else DebounceReleasing $ n - 1
debounce = mealy debounceMachine DebounceWait

mWhen :: a -> Bool -> Maybe a
mWhen a True = Just a
mWhen _ _ = Nothing
(<||>) :: Signal dom (Maybe a) -> Signal dom (Maybe a) -> Signal dom (Maybe a)
(<||>) = liftA2 (<|>)
infixl 3 <||>

i --> f = s
  where
    s = register i $ f <$> s
infix -->
