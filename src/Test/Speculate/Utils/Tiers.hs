-- |
-- Module      : Test.Speculate.Utils.Tiers
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Utilities for manipulating tiers of values (of the 'Listable' typeclass).
module Test.Speculate.Utils.Tiers
  ( productsList
  , mapTMaybe
  , uptoT
  , filterTS
  , discardTS
  )
where

import Test.LeanCheck
import Data.Maybe (mapMaybe)

productsList :: [[a]] -> [[a]]
productsList = concat . products . map toTiers

mapTMaybe :: (a -> Maybe b) -> [[a]] -> [[b]]
mapTMaybe f = map (mapMaybe f)

uptoT :: Int -> [[a]] -> [a]
uptoT sz = concat . take sz

-- this passes the size of the a to the selecting function
filterTS :: (Int -> a -> Bool) -> [[a]] -> [[a]]
filterTS p = fts 0
  where
  fts n []       = []
  fts n (xs:xss) = filter (p n) xs : fts (n+1) xss

discardTS :: (Int -> a -> Bool) -> [[a]] -> [[a]]
discardTS p = filterTS ((not .) . p)
