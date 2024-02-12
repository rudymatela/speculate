-- |
-- Module      : Test.Speculate.Utils.Ord
-- Copyright   : (c) 2016-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Utilities for manipulating 'Ordering' values.
module Test.Speculate.Utils.Ord
  ( module Data.Ord
  , compareIndex
  )
where

import Data.Ord
import Data.List (elemIndex)

compareIndex :: Eq a => [a] -> a -> a -> Ordering
compareIndex xs x y =
  case (elemIndex x xs, elemIndex y xs) of
    (Just  i, Just  j) -> i `compare` j
    (Nothing, Just  _) -> GT
    (Just  _, Nothing) -> LT
    _                  -> EQ
