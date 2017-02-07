{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Eval where

import Data.HVect as HL

import Exp

type family TMap tfun (as :: [*]) :: [*] where
    TMap _ '[] = '[]
    TMap tfun (a ': as) = (tfun a) ': (TMap tfun as)

lookup :: Elem ctx t -> HL.HVect (TMap f ctx) -> f t
lookup EZ = HL.head
lookup (ES ind) = Eval.lookup ind . HL.tail

arith :: (Num a) => Val (Exp.Real a) -> ArithOp (Exp.Real a) b -> Val (Exp.Real a) -> Val b
arith (RealVal x1) Plus (RealVal x2) = RealVal $ x1 + x2

eval :: Exp ctx t -> HL.HVect (TMap Val ctx) -> Val t
eval (Var ind) env = Eval.lookup ind env
eval (Arith e1 op e2) env = arith (eval e1 env) op (eval e2 env)
eval (RealE (Exp.Real v)) _ = RealVal v
eval (BoolE v) _ = BoolVal v

top_eval :: Exp '[] t -> Val t
top_eval exp = eval exp HL.HNil
