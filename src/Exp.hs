{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Exp where

-- | @Elem xs x@ is evidence that @x@ is in the list @xs@.
-- @EZ :: Elem xs x@ is evidence that @x@ is the first element of @xs@.
-- @ES ev :: Elem xs x@ is evidence that @x@ is one position later in
-- @xs@ than is indicated in @ev@
data Elem :: [a] -> a -> * where
  EZ :: Elem (x ': xs) x
  ES :: Elem xs x -> Elem (y ': xs) x

-- | @Exp ctx ty@ is a well-typed expression of type @ty@ in context
-- @ctx@. Note that a context is a list of types, where a type's index
-- in the list indicates the de Bruijn index of the associated term-level
-- variable.
data Exp :: [*] -> * -> * where
  Var   :: Elem ctx ty -> Exp ctx ty
  Lam   :: Exp (arg ': ctx) res -> Exp ctx (arg -> res)
  App   :: Exp ctx (arg -> res) -> Exp ctx arg -> Exp ctx res
  Arith :: (Ord a, Fractional a) => Exp ctx (Exp.Real a) -> ArithOp (Exp.Real a) b -> Exp ctx (Exp.Real a) -> Exp ctx b
  Cond  :: Exp ctx Bool -> Exp ctx ty -> Exp ctx ty -> Exp ctx ty
  Fix   :: Exp ctx (ty -> ty) -> Exp ctx ty
  RealE :: (Exp.Real a) -> Exp ctx (Exp.Real a)
  BoolE :: Bool -> Exp ctx Bool

-- | An @ArithOp tfun@ is an operator on numbers of type @a@ that
-- produces a result of type @tfun a@
data ArithOp inp outp where
  Plus, Minus, Times, Divide             :: ArithOp a a
  Less, LessE, Greater, GreaterE, Equals :: ArithOp a Bool

-- | Classifies types that can be values of glambda expressions
class Value t where
  -- | Well-typed closed values. Encoded as a data family with newtype
  -- instances in order to avoid runtime checking of values
  data Val t

  -- | Convert a glambda value back into a glambda expression
  val :: Val t -> Exp '[] t

newtype Real a = Real a

instance Value (Exp.Real a) where
  newtype Val (Exp.Real a) = RealVal a deriving (Eq, Show)
  val (RealVal n) = RealE (Exp.Real n)

instance Value Bool where
  newtype Val Bool = BoolVal Bool deriving (Eq, Show)
  val (BoolVal b) = BoolE b

instance Value (a -> b) where
  newtype Val (a -> b) = LamVal (Exp '[a] b)
  val (LamVal body) = Lam body
