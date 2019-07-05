-- |
-- Module      : Test.Speculate.Utils.Class
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- An equivalence class type and functions to manipulate it.
module Test.Speculate.Utils.Class
  ( merge
  , mergesOn
  , mergesThat
  , rep
  , map
  , fromRep
  , Class
  )
where

import Data.Function (on)
import Data.List (partition)
import Prelude hiding (map)
import qualified Prelude as P (map)

type Class a = (a,[a])

map :: (a -> b) -> Class a -> Class b
map f (x,xs) = (f x, P.map f xs)

rep :: Class a -> a
rep (x,_) = x

fromRep :: a -> Class a
fromRep x = (x,[])

mergesOn :: Eq b => (a -> b) -> [Class a] -> [Class a]
mergesOn f = P.map (map fst)
           . mergesThat ((==) `on` snd)
           . P.map (map $ \x -> (x, f x))

mergesThat :: (a -> a -> Bool) -> [Class a] -> [Class a]
mergesThat _     []     = []
mergesThat (===) (c:cs) = foldl merge c cs' : mergesThat (===) cs''
  where
  (cs',cs'') = partition (\c' -> rep c === rep c') cs

merge :: Class a -> Class a -> Class a
merge (x,xs) (y,ys) = (x,xs ++ y:ys)
