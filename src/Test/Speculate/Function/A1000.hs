-- |
-- Module      : Test.Speculate.Function.A1000
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- This module exports a Listable instance for functions along with two toy Eq
-- and Ord instances for functions based on 1000 sample return values.
module Test.Speculate.Function.A1000 () where

import Test.Speculate
import Test.LeanCheck.Function.List (areEqualFor, compareFor)

instance (Listable a, Eq b) => Eq (a -> b) where
  (==) = areEqualFor 1000

instance (Listable a, Ord b) => Ord (a -> b) where
  compare = compareFor 1000
