module Test.Speculate.Utils.Memoize
  ( memory
  , memoryFor
  , withMemory
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

memory :: (Listable a, Ord a) => (a -> b) -> Map a b
memory = memoryFor defaultMemory

memoryFor :: (Listable a, Ord a) => Int -> (a -> b) -> Map a b
memoryFor n f = foldr (uncurry M.insert) M.empty . take n $ map (\x -> (x, f x)) list
