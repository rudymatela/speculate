-- Test library
import Test

-- Functions under test
import Test.Speculate.Expr

-- Helper functions
import Data.Functor ((<$>)) -- for GHC < 7.10

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

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
  where ffE = value "f" (undefined :: Int -> Int -> Int)

hh5 :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr
hh5 e1 e2 e3 e4 e5 = hhE :$ e1 :$ e2 :$ e3 :$ e4 :$ e5
  where hhE = value "h" (undefined :: Int -> Int -> Int -> Int -> Int -> Int)

hh7 :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
hh7 e1 e2 e3 e4 e5 e6 e7 = hhE :$ e1 :$ e2 :$ e3 :$ e4 :$ e5 :$ e6 :$ e7
  where hhE = value "h" (undefined :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int)
