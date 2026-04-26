module Calculator.Prelude where

import Clash.Prelude

type NCalcValueBits = 32
-- ceil (CalcValueBits / 3), add 2 to accommodate that Div here rounds down not up
type NBCDDigits = Div (NCalcValueBits + 3 - 1) 3
type NBCDBits = NBCDDigits * 4
data CalcValue = CalcValue
  { valIsNegative :: Bool
  , valNumerator :: Unsigned NCalcValueBits
  , valDenominator :: Unsigned NCalcValueBits
  } deriving (Eq, Ord, Show, Generic, NFDataX)
calcValueZero :: CalcValue
calcValueZero = CalcValue False 0 1

withPacking :: BitPack a => (BitVector (BitSize a) -> BitVector (BitSize a)) -> a -> a
withPacking f a = unpack $ f $ pack a

withUnPacking :: BitPack a => (a -> a) -> (BitVector (BitSize a) -> BitVector (BitSize a))
withUnPacking f a = pack $ f $ unpack a

createDomain vSystem{vName="DomMain", vResetPolarity=ActiveLow}

withGenClockResetEnable :: KnownDomain dom => (HiddenClockResetEnable dom => r) -> r
withGenClockResetEnable s = exposeClockResetEnable s clockGen resetGen enableGen

(##) :: Functor f => f a -> (a -> b)-> f b
fa ## f = f <$> fa
infixr 2 ##
($:) :: (a -> b) -> a -> b
($:) = ($:)
infixr 6 $:

inlineMealy initialState bIn transfer = mealy transfer initialState bIn
testTransfer
  :: (s -> i -> (s, o))
  -> (s -> i -> (s, (s, o)))
testTransfer transfer s i = let (s', o) = transfer s i in (s', (s', o))
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
