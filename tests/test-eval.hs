-- Test library
import Test

-- Functions under test
import Test.Speculate.Expr.Ground
import Test.Speculate.Expr

-- TODO: rename test-eval to test-ground

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n' $ \e -> if isEq preludeInstances e
                      then e === e
                      else e =/= e
  ,                                    xx -+- yy === yy -+- xx
  , holds n' $ \(IntE e1) (IntE e2) -> e1 -+- e2 === e2 -+- e1
  ,                                    xx -+- yy =/= xx -+- xx
  ,                                      abs' xx === abs' (abs' xx)
  ,                      abs' (xx -+- abs' xx) === (xx -+- abs' xx) -- 2*x or 0
  , holds n' $ \e1 e2 -> typ e1 /= typ e2 ==> e1 =/= e2

  , holds n' $ \e1 e2 -> case equation preludeInstances e1 e2 of
                           Just e1e2 -> condEqual preludeInstances 500 e1e2 e1 e2
                           Nothing   -> True
  , holds n' $ \e1 e2 -> case equation preludeInstances e1 e2 of
                           Just e1e2 -> condEqualM preludeInstances 500 0 e1e2 e1 e2
                           Nothing   -> True
  , fails n' $ \e1 e2 -> case equation preludeInstances e1 e2 of
                           Just e1e2 -> condEqualM preludeInstances 500 500 e1e2 e1 e2
                           Nothing   -> True

  , trueBinds preludeInstances 500 (xx -==- zero) == [[(xx,zero)]]
  , trueBinds preludeInstances 500 (xx -==- one)  == [[(xx,one)]]
  , trueBinds preludeInstances 500 ((xx -==- one) -&&- (yy -==- zero))  == [[(xx,one),(yy,zero)]]

  , holds n $ ordOK -:> int
  , holds n $ ordOK -:> ()
  , holds n $ ordOK -:> bool
  , holds n $ ordOK -:> [int]
  ]
  where
  n' = n `div` 50
  x === y = equal preludeInstances 500 x y
  infix 4 ===
  x =/= y = not (x === y)
  infix 4 =/=

(*==*), (*/=*), (*<=*), (*<*) :: (Show a, Typeable a) => a -> a -> Bool
x *==* y = eval undefined $ showConstant x -==- showConstant y
x */=* y = eval undefined $ showConstant x -/=- showConstant y
x *<=* y = eval undefined $ showConstant x -<=- showConstant y
x *<*  y = eval undefined $ showConstant x -<-  showConstant y

eqOK :: (Eq a, Show a, Typeable a) => a -> a -> Bool
eqOK x y =  (x *==* y) == (x == y)
         && (x */=* y) == (x /= y)

ordOK :: (Eq a, Ord a, Show a, Typeable a) => a -> a -> Bool
ordOK x y =  eqOK x y
          && (x *<=* y) == (x <= y)
          && (x *<*  y) == (x <  y)
