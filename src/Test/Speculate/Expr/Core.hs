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

  , falseE
  )
where

import Data.Haexpress
import Test.Speculate.Utils.Typeable

constant :: Typeable a => String -> a -> Expr
constant = value

showConstant :: (Typeable a, Show a) => a -> Expr
showConstant = val

lexicompare :: Expr -> Expr -> Ordering
lexicompare = lexicompareBy compare

lexicompareBy :: (Expr -> Expr -> Ordering) -> Expr -> Expr -> Ordering
lexicompareBy compareConstants = cmp
  where
  e1 `cmp` e2 | isConst e1 && isConst e2             =  e1 `compareConstants` e2
  e1 `cmp` e2 | etyp e1 /= etyp e2                   =  etyp e1 `compareTyE` etyp e2
  e1@(Value ('_':s1) _) `cmp` e2@(Value ('_':s2) _)  =  s1 `compare` s2
  (f :$ x)        `cmp` (g :$ y)                     =  f  `cmp` g <> x `cmp` y
  (_ :$ _)        `cmp` _                            =  GT
  _               `cmp` (_ :$ _)                     =  LT
  _               `cmp` Value ('_':_) _              =  GT
  Value ('_':_) _ `cmp` _                            =  LT
  -- Var < Constants < Apps
  -- the above function exists internally on Haexpress as well.

falseE :: Expr
falseE = showConstant False

-- TODO: get rid of this at some point:
compareTyE :: Either (TypeRep, TypeRep) TypeRep
           -> Either (TypeRep, TypeRep) TypeRep
           -> Ordering
compareTyE (Left (l1,l2)) (Left (r1,r2))  =  l1 `compareTy` r1 <> l2 `compareTy` r2
compareTyE (Right l)      (Right r)       =  l `compareTy` r
compareTyE l              r               =  l `compare` r

countVars :: Expr -> [(Expr,Int)]
countVars e = map (\e' -> (e',length . filter (== e') $ vars e)) $ nubVars e

unrepeatedVars :: Expr -> Bool
unrepeatedVars = all (\(_,n) -> n == 1) . countVars

-- Is this espression an assignment of a variable to a value?
isAssignment :: Expr -> Bool
isAssignment ((Value "==" _ :$ Value ('_':_) _) :$ e2) = True
isAssignment ((Value "==" _ :$ e1) :$ Value ('_':_) _) = True
isAssignment _ = False
-- TODO: use isVar above?

isConstantNamed :: Expr -> String -> Bool
e@(Value n' _) `isConstantNamed` n = isConst e && n' == n
