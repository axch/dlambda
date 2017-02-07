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
  ]
    where two = RealE $ Exp.Real (2 :: Double)
          three = RealE $ Exp.Real (3 :: Double)
          ident = Lam (Var EZ)

main :: IO ()
main = do
  Counts { failures = f, errors = e } <- runTestTT basics
  if f + e > 0 then
      exitWith $ ExitFailure $ f + e
  else
      exitSuccess
