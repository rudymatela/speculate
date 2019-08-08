-- |
-- Module      : Test.Speculate.Utils.Typeable
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Utilities to manipulate 'TypeRep's (of 'Typeable' values).
module Test.Speculate.Utils.Typeable
  ( module Data.Express.Utils.Typeable
  , typesIn
  )
where

import Data.Express.Utils.Typeable
import Data.List

typesIn :: TypeRep -> [TypeRep]
typesIn t  =  nubSortBy compareTy $ tin t []
  where
  tin :: TypeRep -> [TypeRep] -> [TypeRep]
  tin t  =  foldr (.) (t:) (map tin ts)  where  (_,ts)  =  splitTyConApp t

-- temporary
nubSortBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortBy cmp  =  nnub . sortBy cmp
  where
  x -==- y  =  x `cmp` y == EQ
  -- linear nub of adjacent values
  nnub [] = []
  nnub [x] = [x]
  nnub (x:xs) = x : nnub (dropWhile (-==-x) xs)
