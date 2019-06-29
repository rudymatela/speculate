-- Test library
import Test

-- Functions under test
import Test.Speculate.Misc

-- Utils
import Test.Speculate.Expr
import Test.Speculate.Utils

main :: IO ()
main = mainTest tests 10000

-- NOTE: remember that in the expressions returned by functions<N> the
-- variables are actually values (bound by the arguments of the returned
-- function)

tests :: Int -> [Bool]
tests n =
  [ True
-- TODO: remove this file altogether later on
  ]
