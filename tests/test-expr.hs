{-# LANGUAGE CPP #-}
-- Test library
import Test
import qualified Test.LeanCheck.Utils as LC (comparison)

-- Functions under test
import Test.Speculate.Expr
import Test.Speculate.Utils
import Data.List (sort)
import Data.Monoid ((<>))
import Data.Functor ((<$>)) -- for GHC < 7.10
import Data.Maybe (isJust)
import Data.Haexpress (depth, size)

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True
  ]
