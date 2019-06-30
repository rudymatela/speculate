-- |
-- Module      : Test.Speculate.Expr
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Utilities for manipulating expressions.
module Test.Speculate.Expr
  ( module Test.Speculate.Expr.Core
  , module Test.Speculate.Expr.Ground
  , module Test.Speculate.Expr.Match
  , module Test.Speculate.Expr.Instance
  , module Test.Speculate.Expr.Equate
  )
where

import Test.Speculate.Expr.Core hiding (eqWith)
import Test.Speculate.Expr.Ground
import Test.Speculate.Expr.Match
import Test.Speculate.Expr.Instance
import Test.Speculate.Expr.Equate
