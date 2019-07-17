-- |
-- Module      : Test.Speculate.Function.A100
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- This module exports a Listable instance for functions along with two toy Eq
-- and Ord instances for functions based on 100 sample return values.
module Test.Speculate.Function.A100 () where

import Test.Speculate
import Test.Speculate.Function

instance (Listable a, Eq b) => Eq (a -> b) where
  (==) = areEqualFor 100

instance (Listable a, Ord b) => Ord (a -> b) where
  compare = compareFor 100
