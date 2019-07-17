-- |
-- Module      : Test.Speculate.Expr.Equate
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- This module exports
--   smart constructors,
--   smart destructors
-- and queries over
--   equations,
--   inequations
--   and conditional equations.
module Test.Speculate.Expr.Equate
  ( unEquation
  , isEquation
  , unComparison
  , mkImplication
  , unImplication
  , mkConditionalEquation
  , unConditionalEquation
  )
where

import Test.LeanCheck ((==>))
import Data.List ((\\))
import Test.Speculate.Utils
import Test.Speculate.Expr.Core
import Test.Speculate.Expr.Instance

unEquation :: Expr -> (Expr,Expr)
unEquation ((Value "==" _ :$ e1) :$ e2) = (e1,e2)
unEquation _ = error "unEquation: not an equation!"

isEquation :: Expr -> Bool
isEquation ((Value "==" _ :$ e1) :$ e2) = True
isEquation _ = False

unComparison :: Expr -> (Expr,Expr)
unComparison ((Value "compare"  _ :$ e1) :$ e2) = (e1,e2)
unComparison ((Value "<"        _ :$ e1) :$ e2) = (e1,e2)
unComparison ((Value "<="       _ :$ e1) :$ e2) = (e1,e2)
unComparison ((Value ">"        _ :$ e1) :$ e2) = (e1,e2)
unComparison ((Value ">="       _ :$ e1) :$ e2) = (e1,e2)
unComparison _ = error "unComparisonL: not a compare/(<)/(<=)/(>)/(>=) application"

mkImplication :: Expr -> Expr -> Maybe Expr
mkImplication e1 e2
  | typ e1 == boolTy = implicationE :$ e1 $$ e2
  | otherwise        = Nothing
  where
  implicationE = value "==>" (==>)

unImplication :: Expr -> (Expr,Expr)
unImplication ((Value "==>" _ :$ e1) :$ e2) = (e1,e2)
unImplication _ = error "unImplication: not an implication"

mkConditionalEquation :: Instances -> Expr -> Expr -> Expr -> Maybe Expr
mkConditionalEquation ti pre e1 e2 = (pre `mkImplication`) =<< mkEquation ti e1 e2

unConditionalEquation :: Expr -> (Expr,Expr,Expr)
unConditionalEquation ((Value "==>" _ :$ pre) :$ ((Value "==" _ :$ e1) :$ e2)) = (pre,e1,e2)
unConditionalEquation _ = error "unConditionalEquation: not an equation with side condition"
