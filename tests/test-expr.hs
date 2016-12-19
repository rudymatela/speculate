{-# LANGUAGE CPP #-}
-- Test library
import Test
import qualified Test.LeanCheck.Utils as LC (comparison)

-- Functions under test
import Test.Speculate.Expr
import Test.Speculate.Utils
import Data.List (sort)
import Data.Functor ((<$>)) -- for GHC < 7.10
import Data.Typeable (typeOf)

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True
  
  , vals (xx -+- yy) == [plusE]
  , vals (xx -+- (yy -+- zz)) == [plusE]
  , vals (zero -+- one) =$ sort $= [zero, one, plusE]
  , vals ((zero -+- abs' zero) -+- (ord' aa -+- ord' cc))
      =$ sort $= [zero, aa, absE, plusE, ordE]
  , holds n $ \e1 e2 -> timesE `elem` vals (e1 -*- e2)


  , arity zero == 0
  , arity xx == 0
  , arity absE == 1
  , arity plusE == 2
  , arity timesE == 2


  , holds n $ okEqOrd -:> expr
  , holds n $ compare ==== compareComplexity
  , holds n $ LC.comparison lexicompare
  , holds n $ LC.comparison compareComplexity

  , holds n $ equivalence (eqExprCommuting [plusE])
  , holds n $ equivalence (eqExprCommuting [timesE])
  , holds n $ equivalence (eqExprCommuting [plusE,timesE])

  , xx -+- yy == xx -+- yy
  , xx -+- yy /= yy -+- xx
  , not $ eqExprCommuting [timesE] (xx -+- yy) (yy -+- xx)
  ,       eqExprCommuting [plusE]  (xx -+- yy) (yy -+- xx)
  ,       eqExprCommuting [plusE]  (zz -+- (xx -+- yy)) ((yy -+- xx) -+- zz)
  ,       eqExprCommuting [plusE,timesE]  (zz -+- (xx -*- yy)) ((yy -*- xx) -+- zz)

  -- Holes < Values < Apps
  , xx < zero
  , zero < zero -+- one
  , xx < xx -+- yy
  , zero < xx -+- yy

  -- Less arity is less
  , zero < absE
  , absE < timesE
  , aa   < ordE
  , ordE < timesE

  , holds n $ \(IntE e1) (IntE e2) -> match (e1 -+- e2) (xx -+- yy) == Just [("y",e2),("x",e1)]
  , holds n $ \(IntE e)            -> match (e -+- e)   (xx -+- xx) == Just [("x",e)]
  , holds n $ \(IntE e1) (IntE e2) -> e1 /= e2 ==> match (e1 -+- e2) (xx -+- xx) == Nothing
  , holds n $ \(IntE e1) (IntE e2) (IntE e3) -> e2 /= e3
                ==> match ((e1 -+- e1) -+- (e2 -+- e3)) (xx -+- (yy -+- yy)) == Nothing
  , holds n $ \(IntE e1) (IntE e2) -> matchWith [("x",e1)] (e1 -+- e2) (xx -+- yy) == Just [("y",e2),("x",e1)]
  , holds n $ \(IntE e1) (IntE e2) -> e1 /= e2 ==> matchWith [("x",e2)] (e1 -+- e2) (xx -+- yy) == Nothing
  , holds n $ \e1 e2 -> e1 `match` e2 == matchWith [] e1 e2

  , unfoldApp (abs' xx)          == [absE, xx]
  , unfoldApp (abs' (xx -+- yy)) == [absE, xx -+- yy]
  , unfoldApp (xx -+- abs' xx)   == [plusE, xx, abs' xx]

  , holds n $ \e -> renameBy id e == e
  , holds n $ \e -> renameBy tail (renameBy ('x':) e) == e
  , renameBy (++ "1") (xx -+- yy) == ((var "x1" int) -+- (var "y1" int))
  , renameBy (\(c:cs) -> succ c:cs) ((xx -+- yy) -+- ord' cc)
                                 == ((yy -+- zz) -+- ord' dd)

  , unification xx yy == Just [("y",xx),("x",yy)]
  , (canonicalize <$> unify xx yy) == Just xx
  , unification zero zero == Just []
  , unification zero one  == Nothing
  , unification xx one == Just [("x",one)]
  , unification (zero -+- xx) (zero -+- one) == Just [("x",one)]
  , unification (zero -+- xx) (yy -+- one) == Just [("x",one),("y",zero)]
  , unify (zero -+- xx) (yy -+- one) == Just (zero -+- one)
  , unification (ff xx) (ff (gg yy)) == Just [("x",gg yy)]
  , unification (ff xx -+- xx) (yy -+- zero) == Just [("x",zero),("y",ff xx)]
  , unify (ff xx -+- xx) (yy -+- zero) == Just (ff zero -+- zero)
  , unification (ff xx) (gg yy) == Nothing
  , unification (ff xx) (ff yy) == unification xx yy
  , (canonicalize <$> unify (negate' (negate' xx) -+- yy) (xx -+- zero))
    == Just (negate' (negate' xx) -+- zero)

  , canonicalize (xx -+- yy)
              == (xx -+- yy)
  , canonicalize (jj -+- (ii -+- ii))
              == (xx -+- (yy -+- yy))
  , canonicalize ((jj -+- ii) -+- (xx -+- xx))
              == ((xx -+- yy) -+- (zz -+- zz))

  , typ zero == typ one
  , typ zero == typ xx
  , typ zero == typ ii
  , typ xx /= typ cc
  , typ xx == typ (ord' cc)
  , holds n $ \(SameTypeE e1 e2) -> typ e1 == typ e2
  , holds n $ \(IntE  e) -> typ e == typ i_
  , holds n $ \(BoolE e) -> typ e == typ b_
  , holds n $ \(CharE e) -> typ e == typ c_
  , holds n $ \(ListE e) -> typ e == typ xxs
  , etyp (xx :$ yy) == Left (i_ :$ i_)
  , etyp (xx :$ (cc :$ yy)) == Left (i_ :$ (c_ :$ i_))
  , etyp ((ff xx) :$ ((ord' cc) :$ (gg yy))) == Left (i_ :$ (i_ :$ i_))
  , holds n $ \(SameTypeE ef eg) (SameTypeE ex ey) -> (etyp (ef :$ ex) == etyp (eg :$ ey))
  , holds n $ \ef eg ex ey -> (etyp ef == etyp eg && etyp ex == etyp ey)
                           == (etyp (ef :$ ex) == etyp (eg :$ ey))
  , holds n $ \e -> case etyp e of
                      Right t -> t == typ e
                      Left  _ -> error "Either Listable Expr is generating ill typed expressions or etyp is wrong!"

  , typs [i_, idE, plusE, timesE] == sort [typ xx, typ idE, typ plusE]
  , typs [idE, plusE, timesE] == sort [typ idE, typ timesE]
  , typs [c_, ordE, idE, plusE, timesE] == sort [typ xx, typ ordE, typ cc, typ idE, typ timesE]
  , exists (n*2) $ \es -> typs es == nubSort (map typ es)
  , exists (n*2) $ \es -> typs es /= nubSort (map typ es)

  , allUnique (take (n`div`10) $ list :: [Expr])
  , allUnique (take (n`div`10) $ map unSameTypeE list)
  , allUnique (take (n`div`10) $ map unIntE list)

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

  , vars (xx -+- yy) == [(intTy,"x"),(intTy,"y")]
  , vars (xx -+- xx) == [(intTy,"x")]
  , vars (xx -+- xx -+- yy) == [(intTy,"x"),(intTy,"y")]
  , vars (yy -+- xx -+- yy) == [(intTy,"x"),(intTy,"y")]

  ,  (xx -+- xx)         < (xx -+- (xx -+- xx))
  , ((xx -+- xx) -+- xx) > (xx -+- (xx -+- xx))
  , xx < yy
  , zero < one
  , xx < zero

  -- If those two ever fail, it is because the instance for Ord TypeRep in
  -- Data.Typeable has changed.  I do rely on this for a "nice" knuth-bendix
  -- order (by prefering less arity).  If this ever changes, I will have to
  -- explicitly compare type arity on Ord Expr.
  -- (update: haha! It has changed from before, and twice!)
  -- TODO: fix order under GHC <= 7.8
#if __GLASGOW_HASKELL__ < 706
  , typeOf ((+) :: Int -> Int -> Int) > typeOf (abs :: Int -> Int)
  , typeOf (abs :: Int -> Int)        < typeOf (0 :: Int)
#elif __GLASGOW_HASKELL__ < 800
  , typeOf ((+) :: Int -> Int -> Int) < typeOf (abs :: Int -> Int)
  , typeOf (abs :: Int -> Int)        < typeOf (0 :: Int)
#else
  , typeOf ((+) :: Int -> Int -> Int) > typeOf (abs :: Int -> Int)
  , typeOf (abs :: Int -> Int)        > typeOf (0 :: Int)
#endif

  , holds n $ \e1 e2 -> e1 `isSub` e2 == (e1 `elem` subexprsV e2)
  ]
