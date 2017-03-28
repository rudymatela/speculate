-- |
-- Module      : Test.Speculate.Utils.Memoize
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Memoization module.
module Test.Speculate.Utils.Memoize
  ( memory,     memory2
  , memoryFor,  memory2For
  , withMemory, withMemory2
  )
where

import qualified Data.Map as M
import Data.Map (Map)
import Test.LeanCheck (Listable(..))
import Data.Maybe (fromMaybe)

defaultMemory :: Int
defaultMemory = 2520 -- 2^3 * 3^2 * 5 * 7

{- those don't work, GHC wont cache them
memoize :: (Listable a, Ord a) => (a -> b) -> a -> b
memoize = memoizeFor defaultMemory

memoizeFor :: (Listable a, Ord a) => Int -> (a -> b) -> a -> b
memoizeFor n f x = fromMaybe (f x) (M.lookup x m)
  where
  m = memoryFor n f
-}

withMemory :: Ord a => (a -> b) -> Map a b -> a -> b
withMemory f m x = fromMaybe (f x) (M.lookup x m)

withMemory2 :: (Ord a, Ord b) => (a -> b -> c) -> Map (a,b) c -> a -> b -> c
withMemory2 f m = curry (uncurry f `withMemory` m)

memory :: (Listable a, Ord a) => (a -> b) -> Map a b
memory = memoryFor defaultMemory

memory2 :: (Listable a, Listable b, Ord a, Ord b) => (a -> b -> c) -> Map (a,b) c
memory2 = memory2For defaultMemory

memoryFor :: (Listable a, Ord a) => Int -> (a -> b) -> Map a b
memoryFor n f = foldr (uncurry M.insert) M.empty . take n $ map (\x -> (x, f x)) list

memory2For :: (Listable a, Listable b, Ord a, Ord b)
           => Int -> (a -> b -> c) -> Map (a,b) c
memory2For n f = memoryFor n (uncurry f)
