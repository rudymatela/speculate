module Test.Speculate.Utils.Ord
  ( module Data.Ord
  , compareIndex
  )
where

import Data.Ord
import Data.List (findIndex)

compareIndex :: Eq a => [a] -> a -> a -> Ordering
compareIndex xs x y =
  case (findIndex (== x) xs, findIndex (== y) xs) of
    (Just  i, Just  j) -> i `compare` j
    (Nothing, Just  _) -> GT
    (Just  _, Nothing) -> LT
    _                  -> EQ
