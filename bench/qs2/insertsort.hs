{-# LANGUAGE DataKinds #-}
import QuickSpec hiding (insert)
import Data.List (sort, insert)
import Test.QuickCheck
import Data.Dynamic
import Data.Proxy

main = quickSpec signature
  { maxTermSize = Just 5 -- 9
  , constants =
      [ constant "[]" ([] :: [Int])
      , constant ":"  ((:) :: Int -> [Int] -> [Int])
      , constant "++" ((++) :: [Int] -> [Int] -> [Int])
      , constant "insert" (insert :: Int -> [Int] -> [Int])
      , constant "sort"   (sort :: [Int] -> [Int])
      ]
  , predicates =
      [ predicate (undefined :: Proxy "<=") ((<=) :: Int -> Int -> Bool)
      , predicate (undefined :: Proxy "<")  ((<)  :: Int -> Int -> Bool)
      ]
  }
