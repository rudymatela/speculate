-- |
-- Module      : Test.Speculate.Function
-- Copyright   : (c) 2016-2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- If should import this if you want Speculate to treat functions as data
-- values.  This exports a Listable instance for functions an a facility to
-- define Eq and Ord instances for functions.
--
-- Please see the @fun@ and @monad@ examples from Speculate's source
-- repository for more details.
module Test.Speculate.Function
  ( funToList
  , areEqualFor
  , compareFor
  )
where

import Test.Speculate
import Test.LeanCheck.Function()
import Test.LeanCheck.Error (errorToNothing)
import Data.Function (on)

funToList :: Listable a => (a -> b) -> [Maybe b]
funToList f = map (errorToNothing . f) list

-- | This function can be used to define an Eq instance for functions based on
--   testing and equality of returned values, like so:
--
-- > instance (Listable a, Eq b) => Eq (a -> b) where
-- >   (==) = areEqualFor 100
areEqualFor :: (Listable a, Eq b) => Int -> (a -> b) -> (a -> b) -> Bool
areEqualFor n = (==) `on` (take n . funToList)

-- | This function can be used to define an Ord instance for functions based on
--   testing and ordering of returned values, like so:
--
-- > instance (Listable a, Ord b) => Ord (a -> b) where
-- >   compare = compareFor 100
compareFor :: (Listable a, Ord b) => Int -> (a -> b) -> (a -> b) -> Ordering
compareFor n = compare `on` (take n . funToList)
