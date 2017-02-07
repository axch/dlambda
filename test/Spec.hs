import System.Exit

import Test.HUnit

import Exp
import Eval

basics :: Test
basics = test
  [ (top_eval (BoolE True)) @?= BoolVal True
  , (top_eval (RealE (3 :: Double))) @?= DoubleVal 3]

main :: IO ()
main = do
  Counts { failures = f, errors = e } <- runTestTT basics
  if f + e > 0 then
      exitWith $ ExitFailure $ f + e
  else
      exitSuccess
