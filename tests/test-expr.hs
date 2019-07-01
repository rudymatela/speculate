{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- Travis
{-# LANGUAGE CPP #-}
-- Test library
import Test
import qualified Test.LeanCheck.Utils as LC (comparison)

-- Functions under test
import Test.Speculate.Expr
import Test.Speculate.Utils
import Test.Speculate.Reason (emptyThy)
import Data.List (sort)
import Data.Functor ((<$>)) -- for GHC < 7.10
import Data.Typeable (typeOf)
import Data.Maybe (isJust)
import Data.Haexpress (depth, size)

-- for Travis:
deriving instance Typeable Thyght
deriving instance Typeable Equation

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True

  , nubConsts (xx -+- yy) == [plusE]
  , nubConsts (xx -+- (yy -+- zz)) == [plusE]
  , nubConsts (zero -+- one) =$ sort $= [zero, one, plusE]
  , nubConsts ((zero -+- abs' zero) -+- (ord' aa -+- ord' cc))
      =$ sort $= [zero, aa, absE, plusE, ordE]
  , holds n $ \e1 e2 -> timesE `elem` consts (e1 -*- e2)


  , holds n $ okEqOrd -:> (undefined :: Expr)
  , holds n $ compare ==== (compareComplexity <> lexicompare)
  , holds n $ LC.comparison lexicompare
  , holds n $ LC.comparison compareComplexity

  , holds n $ \(FunE e1) (FunE e2) e3 -> let cmp = lexicompare
                                         in typ e1 == typ e2 && isJust (e1 $$ e3) && isJust (e2 $$ e3)
                                        ==> e1 `cmp` e2 == (e1 :$ e3) `cmp` (e2 :$ e3)
  , holds n $ \(FunE e1) (FunE e2) e3 -> let cmp = lexicompareBy (flip compare)
                                         in typ e1 == typ e2 && isJust (e1 $$ e3) && isJust (e2 $$ e3)
                                        ==> e1 `cmp` e2 == (e1 :$ e3) `cmp` (e2 :$ e3)

  , xx -+- yy == xx -+- yy
  , xx -+- yy /= yy -+- xx

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
  , constant "id" (id -:>  int)  < constant "id"    (id    -:>  [int])
  , constant "id" (id -:> [int]) < constant "id"    (id    -:> [[int]])
  , constant "id" (id -:>  int)  < constant "sum"   (sum   -:>  [int])
  , constant "id" (id -:>  int)  < constant "(:[])" ((:[]) -:>   int)

  -- precedent types
  , pp < xx
  , cc < xx
  , pp < cc
  , xx < xxs
  , aa < zero
  , Test.true < zero
  , Test.true < aa
  , zero < ll

  -- further precedent types
  , constant "xx" xx < zero
  , constant "xxeqxx" (Equation xx xx) < constant "xx" xx
  , constant "xx" xx < constant "emptyThyght" (Thyght emptyThy)

  , unfoldApp (abs' xx)          == [absE, xx]
  , unfoldApp (abs' (xx -+- yy)) == [absE, xx -+- yy]
  , unfoldApp (xx -+- abs' xx)   == [plusE, xx, abs' xx]

  , holds n $ \e -> renameBy id e == e
  , holds n $ \e -> renameBy tail (renameBy ('x':) e) == e
  , renameBy (++ "1") (xx -+- yy) == (var "x1" int -+- var "y1" int)
  , renameBy (\(c:cs) -> succ c:cs) ((xx -+- yy) -+- ord' cc)
                                 == ((yy -+- zz) -+- ord' dd)

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

  , etyp (xx :$ yy) == Left (typ i_, typ i_)
  , etyp (xx :$ (cc :$ yy)) == Left (typ c_, typ i_)
  , etyp (ff xx :$ (ord' cc :$ gg yy)) == Left (typ i_, typ i_)
  , holds n $ \(SameTypeE ef eg) (SameTypeE ex ey) -> (etyp (ef :$ ex) == etyp (eg :$ ey))
  , holds n $ \ef eg ex ey -> (etyp ef == etyp eg && etyp ex == etyp ey)
                           == (etyp (ef :$ ex) == etyp (eg :$ ey))
  , holds n $ \e -> case etyp e of
                      Right t -> t == typ e
                      Left  _ -> error "Either Listable Expr is generating ill typed expressions or etyp is wrong!"

  , size  zero == 1
  , depth zero == 1
  , size  one  == 1
  , depth one  == 1
  , size  (zero -+- one) == 3
  , depth (zero -+- one) == 2
  , size  (zero -+- (xx -+- yy)) == 5
  , depth (zero -+- (xx -+- yy)) == 3
  , size  (((xx -+- yy) -*- zz) -==- ((xx -*- zz) -+- (yy -*- zz))) == 13
  , depth (((xx -+- yy) -*- zz) -==- ((xx -*- zz) -+- (yy -*- zz))) ==  4
  , depth (xx -*- yy -+- xx -*- zz -==- xx -*- (yy -+- zz)) == 4
  , size  (xx -*- yy -+- xx -*- zz -==- xx -*- (yy -+- zz)) == 13
  , depth (xx -*- yy -+- xx -*- zz) == 3
  , depth (xx -*- (yy -+- zz)) == 3

  , allUnique (take (n`div`10) list :: [Expr])
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

  , vars (xx -+- yy) == [xx, yy]
  , nubVars (xx -+- xx) == [xx]
  , nubVars (xx -+- xx -+- yy) == [xx, yy]
  , nubVars (yy -+- xx -+- yy) == [xx, yy]

  ,  (xx -+- xx)         < (xx -+- (xx -+- xx))
  , ((xx -+- xx) -+- xx) > (xx -+- (xx -+- xx))
  , xx < yy
  , zero < one
  , xx < zero
  ]
