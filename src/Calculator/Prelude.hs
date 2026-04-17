module Calculator.Prelude where

import Clash.Prelude

type CalcValue = (Signed 12, Unsigned 12)
createDomain vSystem{vName="DomMain", vResetPolarity=ActiveLow}

(##) :: Functor f => f a -> (a -> b)-> f b
fa ## f = f <$> fa
infixr 2 ##

inlineMealy initialState bIn transfer = mealy transfer initialState bIn
