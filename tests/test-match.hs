-- Test library
import Test

-- Functions under test
import Test.Speculate.Expr.Match
import Test.Speculate.Expr

-- Helper functions
import Test.Speculate.Utils
import Data.List
import Data.Maybe
import Data.Functor ((<$>)) -- for GHC < 7.10

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

  , exists n $ \(SameTypeE e1 e2) -> unification e1 e2 /= unification e2 e1
  , holds  n $ \(SameTypeE e1 e2) -> unify e1 e2 =$ fmap canonicalize $= unify e2 e1

  , unification xx yy == Just [("x",yy)]
  , (canonicalize <$> unify xx yy) == Just xx
  , unification zero zero == Just []
  , unification zero one  == Nothing
  , unification xx one == Just [("x",one)]
  , unification (zero -+- xx) (zero -+- one) == Just [("x",one)]
  , unification (zero -+- xx) (yy -+- one) == Just [("x",one),("y",zero)]
  , unify (zero -+- xx) (yy -+- one) == Just (zero -+- one)
  , unification (ff xx) (ff (gg yy)) == Just [("x",gg yy)]
  , unification (ff xx -+- xx) (yy -+- zero) == Just [("x",zero),("y",ff zero)]
  , unify (ff xx -+- xx) (yy -+- zero) == Just (ff zero -+- zero)
  , unification (ff xx) (gg yy) == Nothing
  , unification (ff xx) (ff yy) == unification xx yy
  , (canonicalize <$> unify (negate' (negate' xx) -+- yy) (xx -+- zero))
    == Just (negate' (negate' xx) -+- zero)
  , unification (xx -+- one) (one -+- xx) == Just [("x",one)]
  , unification (xx -+- xx) (one -+- one) == Just [("x",one)]
  , unification (zz -+- zz) (xx -+- yy) == Just [("x",yy),("z",yy)]
  , unification (xx    -*- (-+-) xx xx)
                (ff zz -*- (-+-) xx yy) == Just [("y",ff zz),("x",ff zz)]
  ]
  where
  x === y = equal preludeInstances 1000 x y
