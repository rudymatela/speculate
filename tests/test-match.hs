-- Test library
import Test

-- Functions under test
import Test.Speculate.Expr.Match
import Test.Speculate.Expr

-- Helper functions
import Data.Functor ((<$>)) -- for GHC < 7.10

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \e -> renameBy id e == e
  , holds n $ \e -> renameBy tail (renameBy ('x':) e) == e
  , renameBy (++ "1") (xx -+- yy) == (var "x1" int -+- var "y1" int)
  , renameBy (\(c:cs) -> succ c:cs) ((xx -+- yy) -+- ord' cc)
                                 == ((yy -+- zz) -+- ord' dd)

  , holds n $ \(IntE e)            -> e `isInstanceOf` xx
  , holds n $ \(IntE e)            -> abs' e `isInstanceOf` abs' xx
  , holds n $ \(IntE e)            -> (e -+- e) `isInstanceOf` (xx -+- xx)
  , holds n $ \(IntE e1) (IntE e2) -> (e1 -+- e2) `isInstanceOf` (xx -+- yy)
  , holds n $ \(IntE e1) (IntE e2) -> e1 /= e2 ==> not ((e1 -+- e2) `isInstanceOf` (xx -+- xx))
  , holds n $ \e                   -> e /= zero ==> not (e `isInstanceOf` zero)

  ,       (zero -+- one)       `isInstanceOf` (xx -+- yy)
  ,       (zero -+- zero)      `isInstanceOf` (xx -+- yy)
  ,       (yy -+- xx)          `isInstanceOf` (xx -+- yy)
  ,       (zero -+- zero)      `isInstanceOf` (xx -+- xx)
  , not $ (zero -+- one)       `isInstanceOf` (xx -+- xx)
  ,       zero                 `isInstanceOf`          xx
  , not $ xx                   `isInstanceOf`        zero
  ,       (xx -+- (yy -+- xx)) `isInstanceOf` (xx -+- yy)
  ,       (xx -+- (xx -+- xx)) `isInstanceOf` (xx -+- yy)
  , not $ (xx -+- (xx -+- xx)) `isInstanceOf` (xx -+- xx)

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
                (e1 -==- e2) `match` (e3 -==- e4) == foldPair (e1,e2) `match` foldPair (e3,e4)


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
  , unification (foo xx) (foo (goo yy)) == Just [(xx,goo yy)]
  , unification (foo xx -+- xx) (yy -+- zero) == Just [(xx,zero),(yy,foo zero)]
  , unify (foo xx -+- xx) (yy -+- zero) == Just (foo zero -+- zero)
  , unification (foo xx) (goo yy) == Nothing
  , unification (foo xx) (foo yy) == unification xx yy
  , (canonicalize <$> unify (negate' (negate' xx) -+- yy) (zz -+- zero))
    == Just (negate' (negate' xx) -+- zero)
  , unification (xx -+- one) (one -+- xx) == Just [(xx,one)]
  , unification (xx -+- xx) (one -+- one) == Just [(xx,one)]
  , unification (zz -+- zz) (xx -+- yy) == Just [(xx,yy),(zz,yy)]
  , unification (xx     -*- (-+-) xx xx)
                (foo zz -*- (-+-) xx yy) == Just [(yy,foo zz),(xx,foo zz)]

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

ff2 :: Expr -> Expr -> Expr
ff2 e1 e2 = ffE :$ e1 :$ e2
  where ffE = constant "f" (undefined :: Int -> Int -> Int)

hh5 :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr
hh5 e1 e2 e3 e4 e5 = hhE :$ e1 :$ e2 :$ e3 :$ e4 :$ e5
  where hhE = constant "h" (undefined :: Int -> Int -> Int -> Int -> Int -> Int)

hh7 :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
hh7 e1 e2 e3 e4 e5 e6 e7 = hhE :$ e1 :$ e2 :$ e3 :$ e4 :$ e5 :$ e6 :$ e7
  where hhE = constant "h" (undefined :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int)
