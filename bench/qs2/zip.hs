{-# LANGUAGE DataKinds #-}
import QuickSpec hiding (insert)
import Data.List (sort, insert)
import Test.QuickCheck
import Data.Dynamic
import Control.Monad
import Data.Proxy

main = quickSpec signature
  { maxTermSize = Just 7
  , maxTests = Just 500
  , constants =
      [ constant "++" ((++) :: [Int] -> [Int] -> [Int])
      , constant "length" (length :: [Int] -> Int)
      , constant "zip"   (zip :: [Int] -> [Int] -> [(Int,Int)])
      ]
  , predicates =
      [ predicate (undefined :: Proxy "eqLen") eqLen
      ]
  }

eqLen :: [Int] -> [Int] -> Bool
eqLen xs ys  =  length xs == length ys
