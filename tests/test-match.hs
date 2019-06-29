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

  , holds n $ \(IntE e1) (IntE e2) -> match (e1 -+- e2) (xx -+- yy) == Just [(yy,e2),(xx,e1)]
  , holds n $ \(IntE e)            -> match (e -+- e)   (xx -+- xx) == Just [(xx,e)]
  , holds n $ \(IntE e1) (IntE e2) -> e1 /= e2 ==> match (e1 -+- e2) (xx -+- xx) == Nothing
  , holds n $ \(IntE e1) (IntE e2) (IntE e3) -> e2 /= e3
                ==> match ((e1 -+- e1) -+- (e2 -+- e3)) (xx -+- (yy -+- yy)) == Nothing
  , holds n $ \(IntE e1) (IntE e2) -> matchWith [(xx,e1)] (e1 -+- e2) (xx -+- yy) == Just [(yy,e2),(xx,e1)]
  , holds n $ \(IntE e1) (IntE e2) -> e1 /= e2 ==> matchWith [(xx,e2)] (e1 -+- e2) (xx -+- yy) == Nothing
  , holds n $ \e1 e2 -> e1 `match` e2 == matchWith [] e1 e2
  , holds n $ \(SameTypeE e1 e2) (SameTypeE e3 e4) ->
                not (isFunTy $ typ e1) && not (isFunTy $ typ e3)
                  ==>
                (e1 -==- e2) `match` (e3 -==- e4) == (e1,e2) `match2` (e3,e4)


  , ((xx -+- yy) -+- (yy -+- zz)) //- [(yy, yy -+- zz)]
      == (xx -+- (yy -+- zz)) -+- ((yy -+- zz) -+- zz)

  , (xx -+- yy) // [(yy, yy -+- zz), (xx, xx -+- yy)]
      == (xx -+- yy) -+- (yy -+- zz)

  , sub (xx -+- yy) zero ((xx -+- yy) -+- zz) == (zero -+- zz)
  , sub (xx -+- yy) zero (xx -+- (yy -+- zz)) == (xx -+- (yy -+- zz))

  , holds n $ \(SameTypeE e1 e2) -> sub e1 e2 e1 == e2
  , holds n $ \(IntE e1) (IntE e2) -> sub e1 e2 (e1 -+- e1) == (e2 -+- e2)

  , holds n $ \(SameTypeE e1 e2) -> unify e1 e2 =$ fmap canonicalize $= unify e2 e1

  , unification xx yy == Just [(xx,yy)]
  , (canonicalize <$> unify xx yy) == Just xx
  , unification zero zero == Just []
  , unification zero one  == Nothing
  , unification xx one == Just [(xx,one)]
  , unification (zero -+- xx) (zero -+- one) == Just [(xx,one)]
  , unification (zero -+- xx) (yy -+- one) == Just [(xx,one),(yy,zero)]
  , unify (zero -+- xx) (yy -+- one) == Just (zero -+- one)
  , unification (ff xx) (ff (gg yy)) == Just [(xx,gg yy)]
  , unification (ff xx -+- xx) (yy -+- zero) == Just [(xx,zero),(yy,ff zero)]
  , unify (ff xx -+- xx) (yy -+- zero) == Just (ff zero -+- zero)
  , unification (ff xx) (gg yy) == Nothing
  , unification (ff xx) (ff yy) == unification xx yy
  , (canonicalize <$> unify (negate' (negate' xx) -+- yy) (zz -+- zero))
    == Just (negate' (negate' xx) -+- zero)
  , unification (xx -+- one) (one -+- xx) == Just [(xx,one)]
  , unification (xx -+- xx) (one -+- one) == Just [(xx,one)]
  , unification (zz -+- zz) (xx -+- yy) == Just [(xx,yy),(zz,yy)]
  , unification (xx    -*- (-+-) xx xx)
                (ff zz -*- (-+-) xx yy) == Just [(yy,ff zz),(xx,ff zz)]

  -- The following two tests are adapted from Baader and Snyder:
  -- Example 2.8, Chapter 8, Handbook of Automated Reasoning (page 453).
  , unification (hh5 yy zz (ff2 ii ii) (ff2 jj jj) kk) (hh5 (ff2 xx xx) (ff2 yy yy) jj kk zz)
    == Just [ (ii,xx)
            , (kk,ff2 (ff2 xx xx) (ff2 xx xx))
            , (jj,ff2 xx xx)
            , (zz,ff2 (ff2 xx xx) (ff2 xx xx))
            , (yy,ff2 xx xx)
            ]
  , unification (hh7 yy zz xx' (ff2 ii ii) (ff2 jj jj) (ff2 kk kk) ii')
                (hh7 (ff2 xx xx) (ff2 yy yy) (ff2 zz zz) jj kk ii' xx')
    == Just [ (ii,xx)
            , (ii',ff2 (ff2 (ff2 xx xx) (ff2 xx xx)) (ff2 (ff2 xx xx) (ff2 xx xx)))
            , (kk,ff2 (ff2 xx xx) (ff2 xx xx))
            , (jj,ff2 xx xx)
            , (xx',ff2 (ff2 (ff2 xx xx) (ff2 xx xx)) (ff2 (ff2 xx xx) (ff2 xx xx)))
            , (zz,ff2 (ff2 xx xx) (ff2 xx xx))
            , (yy,ff2 xx xx)
            ]
  ]
