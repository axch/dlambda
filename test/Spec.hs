import System.Exit

import Data.HVect as HL
import Test.HUnit

import Exp
import Eval

basics :: Test
basics = test
  [ (top_eval (BoolE True)) @?= BoolVal True
  , (top_eval three) @?= RealVal 3
  , (top_eval (Arith two Plus three)) @?= RealVal 5
  , (top_eval (App ident three)) @?= RealVal 3
  , (top_eval (Cond (Arith two Less three) two three)) @?= RealVal 2
  , (top_eval (App (Fix fact_k) three)) @?= RealVal 6
  ]
    where one = RealE $ Exp.Real (1 :: Double)
          two = RealE $ Exp.Real (2 :: Double)
          three = RealE $ Exp.Real (3 :: Double)
          ident = Lam (Var EZ)
          fact_k = Lam $ Lam $ Cond (Arith n LessE one) one $
                   Arith n Times (App fact (Arith n Minus one)) where
                       n = Var EZ
                       fact = Var (ES EZ)

main :: IO ()
main = do
  Counts { failures = f, errors = e } <- runTestTT basics
  if f + e > 0 then
      exitWith $ ExitFailure $ f + e
  else
      exitSuccess
