{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Eval where

import Data.HVect as HL

import Exp

lookup :: Elem ctx t -> HL.HVect (TMap f ctx) -> f t
lookup EZ = HL.head
lookup (ES ind) = Eval.lookup ind . HL.tail

apply :: Val (arg -> res) -> Val arg -> Val res
apply (LamVal body env) v = eval body (v HL.:&: env) where

arith :: (Ord a, Fractional a) => Val (Exp.Real a) -> ArithOp (Exp.Real a) b -> Val (Exp.Real a) -> Val b
arith (RealVal x1) Plus     (RealVal x2) = RealVal $ x1 + x2
arith (RealVal x1) Minus    (RealVal x2) = RealVal $ x1 - x2
arith (RealVal x1) Times    (RealVal x2) = RealVal $ x1 * x2
arith (RealVal x1) Divide   (RealVal x2) = RealVal $ x1 / x2
arith (RealVal x1) Less     (RealVal x2) = BoolVal $ x1 < x2
arith (RealVal x1) LessE    (RealVal x2) = BoolVal $ x1 <= x2
arith (RealVal x1) Greater  (RealVal x2) = BoolVal $ x1 > x2
arith (RealVal x1) GreaterE (RealVal x2) = BoolVal $ x1 >= x2
arith (RealVal x1) Equals   (RealVal x2) = BoolVal $ x1 == x2

cond :: Val Bool -> a -> a -> a
cond (BoolVal True)  a _ = a
cond (BoolVal False) _ a = a

eval :: Exp ctx t -> HL.HVect (TMap Val ctx) -> Val t
eval (Var ind) env = Eval.lookup ind env
eval (Lam body) env = LamVal body env
eval (App e1 e2) env = apply (eval e1 env) (eval e2 env)
eval (Arith e1 op e2) env = arith (eval e1 env) op (eval e2 env)
eval (Cond p c a) env = eval (cond (eval p env) c a) env
eval (RealE (Exp.Real v)) _ = RealVal v
eval (BoolE v) _ = BoolVal v

top_eval :: Exp '[] t -> Val t
top_eval exp = eval exp HL.HNil
