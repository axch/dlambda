{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Eval where

import Exp

import Data.HVect as HL

type family TMap tfun (as :: [*]) :: [*] where
    TMap _ '[] = '[]
    TMap tfun (a ': as) = (tfun a) ': (TMap tfun as)

lookup :: Elem ctx t -> HL.HVect (TMap f ctx) -> f t
lookup EZ = HL.head
lookup (ES ind) = Eval.lookup ind . HL.tail

eval :: Exp ctx t -> HL.HVect (TMap Val ctx) -> Val t
eval (Var ind) env = Eval.lookup ind env
