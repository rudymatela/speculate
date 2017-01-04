-- Test library
import Test

-- Functions under test
import Test.Speculate.Expr.Match

-- Helper functions
import Test.Speculate.Utils
import Data.List
import Data.Maybe

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \(IntE e1) (IntE e2) -> match (e1 -+- e2) (xx -+- yy) == Just [("y",e2),("x",e1)]
  , holds n $ \(IntE e)            -> match (e -+- e)   (xx -+- xx) == Just [("x",e)]
  , holds n $ \(IntE e1) (IntE e2) -> e1 /= e2 ==> match (e1 -+- e2) (xx -+- xx) == Nothing
  , holds n $ \(IntE e1) (IntE e2) (IntE e3) -> e2 /= e3
                ==> match ((e1 -+- e1) -+- (e2 -+- e3)) (xx -+- (yy -+- yy)) == Nothing
  , holds n $ \(IntE e1) (IntE e2) -> matchWith [("x",e1)] (e1 -+- e2) (xx -+- yy) == Just [("y",e2),("x",e1)]
  , holds n $ \(IntE e1) (IntE e2) -> e1 /= e2 ==> matchWith [("x",e2)] (e1 -+- e2) (xx -+- yy) == Nothing
  , holds n $ \e1 e2 -> e1 `match` e2 == matchWith [] e1 e2
  , holds n $ \(SameTypeE e1 e2) (SameTypeE e3 e4) ->
                not (isFunTy $ typ e1) && not (isFunTy $ typ e3)
                  ==>
                (e1 -==- e2) `match` (e3 -==- e4) == (e1,e2) `match2` (e3,e4)


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
