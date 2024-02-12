-- |
-- Module      : Test.Speculate.Function.A10
-- Copyright   : (c) 2019-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- This module exports a Listable instance for functions along with two toy Eq
-- and Ord instances for functions based on 10 sample return values.
module Test.Speculate.Function.A10 () where

import Test.Speculate
import Test.LeanCheck.Function ()
import Test.LeanCheck.Function.List (areEqualFor, compareFor)

instance (Listable a, Eq b) => Eq (a -> b) where
  (==) = areEqualFor 10

instance (Listable a, Ord b) => Ord (a -> b) where
  compare = compareFor 10
