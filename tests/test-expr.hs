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
import Data.Monoid ((<>))
import Data.Functor ((<$>)) -- for GHC < 7.10
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

  , holds n $ compare ==== (compareComplexity <> lexicompare)
  , holds n $ LC.comparison lexicompare
  , holds n $ LC.comparison compareComplexity

  , holds n $ \(IntToIntE e1) (IntToIntE e2) (IntE e3) -> let cmp = lexicompare in
                e1 `cmp` e2 == (e1 :$ e3) `cmp` (e2 :$ e3)
  , holds n $ \(IntToIntE e1) (IntToIntE e2) (IntE e3) -> let cmp = lexicompareBy (flip compare) in
                e1 `cmp` e2 == (e1 :$ e3) `cmp` (e2 :$ e3)
  , holds n $ \(BoolToBoolE e1) (BoolToBoolE e2) (BoolE e3) -> let cmp = lexicompare in
                e1 `cmp` e2 == (e1 :$ e3) `cmp` (e2 :$ e3)
  , holds n $ \(BoolToBoolE e1) (BoolToBoolE e2) (BoolE e3) -> let cmp = lexicompareBy (flip compare) in
                e1 `cmp` e2 == (e1 :$ e3) `cmp` (e2 :$ e3)

  -- some tests of order
  , constant "xx" xx < zero
  , constant "xxeqxx" (Equation xx xx) < constant "xx" xx
  , constant "xx" xx < constant "emptyThyght" (Thyght emptyThy)

  , holds n $ \e -> renameBy id e == e
  , holds n $ \e -> renameBy tail (renameBy ('x':) e) == e
  , renameBy (++ "1") (xx -+- yy) == (var "x1" int -+- var "y1" int)
  , renameBy (\(c:cs) -> succ c:cs) ((xx -+- yy) -+- ord' cc)
                                 == ((yy -+- zz) -+- ord' dd)
  ]
