-- |
-- Module      : Test.Speculate.Expr.Core
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Defines the 'Expr' type and basic operations on it.
{-# LANGUAGE DeriveDataTypeable #-} -- for GHC < 7.10
module Test.Speculate.Expr.Core
  ( module Data.Haexpress
  , lexicompare
  , lexicompareBy
  , constant
  , showConstant
  , isConstantNamed
  , unrepeatedVars
  , isAssignment
  )
where

import Data.Haexpress
import Test.Speculate.Utils.Typeable
import Data.Monoid ((<>))

constant :: Typeable a => String -> a -> Expr
constant = value

showConstant :: (Typeable a, Show a) => a -> Expr
showConstant = val

lexicompare :: Expr -> Expr -> Ordering
lexicompare = lexicompareBy compare

lexicompareBy :: (Expr -> Expr -> Ordering) -> Expr -> Expr -> Ordering
lexicompareBy compareConstants  =  cmp
  where
  (f :$ x) `cmp` (g :$ y)  =  f  `cmp` g <> x `cmp` y
  (_ :$ _) `cmp` _         =  GT
  _        `cmp` (_ :$ _)  =  LT
  e1 `cmp` e2  =  case (isVar e1, isVar e2) of
    (True,  True)  -> let Value n1 _ = e1
                          Value n2 _ = e2
                      in typ e1 `compareTy` typ e2 <> n1 `compare` n2
    (False, True)  -> GT
    (True,  False) -> LT
    (False, False) -> e1 `compareConstants` e2
  -- Var < Constants < Apps

countVars :: Expr -> [(Expr,Int)]
countVars e = map (\e' -> (e',length . filter (== e') $ vars e)) $ nubVars e

unrepeatedVars :: Expr -> Bool
unrepeatedVars = all (\(_,n) -> n == 1) . countVars

-- Is this espression an assignment of a variable to a value?
isAssignment :: Expr -> Bool
isAssignment ((Value "==" _ :$ e1) :$ e2) = isVar e1 || isVar e2
isAssignment _ = False

isConstantNamed :: Expr -> String -> Bool
e@(Value n' _) `isConstantNamed` n = isConst e && n' == n
_ `isConstantNamed` _ = False
