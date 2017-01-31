-- Test library
import Test

-- Functions under test
import Test.Speculate.Expr.Ground
import Test.Speculate.Utils

import Data.Maybe

-- TODO: rename test-eval to test-ground

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n' $ \e -> if isComparable basicTypeInfo e
                      then e === e
                      else e =/= e
  ,                                    xx -+- yy === yy -+- xx
  , holds n' $ \(IntE e1) (IntE e2) -> e1 -+- e2 === e2 -+- e1
  ,                                    xx -+- yy =/= xx -+- xx
  ,                                      abs' xx === abs' (abs' xx)
  ,                      abs' (xx -+- (abs' xx)) === (xx -+- abs' xx) -- 2*x or 0
  , holds n' $ \e1 e2 -> typ e1 /= typ e2 ==> e1 =/= e2

  , holds n' $       zero //= one
  , holds n' $ not $   xx //= yy

  , holds n' $ \e1 e2 -> case equation basicTypeInfo e1 e2 of
                           Just e1e2 -> condEqual basicTypeInfo 500 0 e1e2 e1 e2
                           Nothing   -> True
  , holds n' $ \e1 e2 -> case equation basicTypeInfo e1 e2 of
                           Just e1e2 -> condEqual basicTypeInfo 500 1 e1e2 e1 e2
                           Nothing   -> True
  , fails n' $ \e1 e2 -> case equation basicTypeInfo e1 e2 of
                           Just e1e2 -> condEqual basicTypeInfo 500 500 e1e2 e1 e2
                           Nothing   -> True

  , trueBinds basicTypeInfo 500 (xx -==- zero) == [[("x",zero)]]
  , trueBinds basicTypeInfo 500 (xx -==- one)  == [[("x",one)]]
  , trueBinds basicTypeInfo 500 ((xx -==- one) -&&- (yy -==- zero))  == [[("x",one),("y",zero)]]
  ]
  where
  n' = n `div` 50
  x === y = equal basicTypeInfo 500 x y
  infix 4 ===
  x =/= y = not (x === y)
  infix 4 =/=
  x //= y = inequal basicTypeInfo 500 x y
