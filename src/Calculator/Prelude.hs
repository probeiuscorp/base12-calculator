module Calculator.Prelude where

import Clash.Prelude

(##) :: Functor f => f a -> (a -> b)-> f b
fa ## f = f <$> fa
infixr 2 ##

inlineMealy initialState bIn transfer = mealy transfer initialState bIn
