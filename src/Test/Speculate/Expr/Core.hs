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
lexicompareBy compareConstants = cmp
  where
  e1@(Value ('_':s1) _) `cmp` e2@(Value ('_':s2) _)  =  typ e1 `compareTy` typ e2 <> s1 `compare` s2
  (f :$ x)        `cmp` (g :$ y)                     =  f  `cmp` g <> x `cmp` y
  (_ :$ _)        `cmp` _                            =  GT
  _               `cmp` (_ :$ _)                     =  LT
  _               `cmp` Value ('_':_) _              =  GT
  Value ('_':_) _ `cmp` _                            =  LT
  e1@(Value _ _)  `cmp` e2@(Value _ _)               =  e1 `compareConstants` e2
  -- Var < Constants < Apps
  -- the above function exists internally on Haexpress as well.

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
