-- Test library
import Test

-- Functions under test
import Speculate.Expr.Match

-- Helper functions
import Speculate.Utils
import Data.List
import Data.Maybe

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , assign "y" (yy -+- zz) ((xx -+- yy) -+- (yy -+- zz))
      == (xx -+- (yy -+- zz)) -+- ((yy -+- zz) -+- zz)

  , ((xx -+- yy) -+- (yy -+- zz)) `assigning` [("y",yy -+- zz)]
      == (xx -+- (yy -+- zz)) -+- ((yy -+- zz) -+- zz)

  , (xx -+- yy) `assigning` [("y",yy -+- zz),("x",xx -+- yy)]
      == (xx -+- yy) -+- (yy -+- zz)

  , sub (xx -+- yy) zero ((xx -+- yy) -+- zz) == (zero -+- zz)
  , sub (xx -+- yy) zero (xx -+- (yy -+- zz)) == (xx -+- (yy -+- zz))

  , holds n $ \(SameTypeE e1 e2) -> sub e1 e2 e1 == e2
  , holds n $ \(IntE e1) (IntE e2) -> sub e1 e2 (e1 -+- e1) == (e2 -+- e2)
  ]
  where
  x === y = equal basicTypeInfo 1000 x y
