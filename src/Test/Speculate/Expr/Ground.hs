-- |
-- Module      : Test.Speculate.Expr.Ground
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Generate and evaluate ground values of expressions.
module Test.Speculate.Expr.Ground
  ( grounds
  , groundBinds
  , equal
  , lessOrEqual
  , less
  , inequal
  , isTrue
  , isFalse
  , condEqual
  , condEqualM
  , trueRatio
  )
where

import Test.Speculate.Expr.Core
import Test.Speculate.Expr.Instance
import Test.Speculate.Expr.Equate
import Test.LeanCheck
import Test.LeanCheck.Error (errorToFalse)
import Data.Ratio
import Data.Functor ((<$>)) -- for GHC < 7.10
import Data.Maybe (fromMaybe)

-- | List all possible valuations of an expression (potentially infinite).
--   In pseudo-Haskell:
--
-- > take 3 $ grounds (lookupTiers preludeInstances) ((x + x) + y)
-- >   == [(0 + 0) + 0, (0 + 0) + 1, (1 + 1) + 0]
--
-- Note this function will return an empty list when a 'Listable' instance is
-- not found in the 'Instances' list.
grounds :: (Expr -> [[Expr]]) -> Expr -> [Expr]
grounds tiersFor e = (e //-) <$> groundBinds tiersFor e

-- | List all possible variable bindings to an expression
--
-- > take 3 $ groundBinds (lookupTiers preludeInstances) ((x + x) + y)
-- >   == [ [("x",0),("y",0)]
-- >      , [("x",0),("y",1)]
-- >      , [("x",1),("y",0)] ]
groundBinds :: (Expr -> [[Expr]]) -> Expr -> [Binds]
groundBinds tiersFor e =
  concat $ products [mapT ((,) v) (tiersFor v) | v <- nubVars e]

-- | Are two expressions equal for a given number of tests?
equal :: Instances -> Int -> Expr -> Expr -> Bool
-- equal ti _ e1 e2 | e1 == e2 = isComparable ti e1 -- optional optimization
equal ti n = isTrueComparison (take n . grounds (lookupTiers ti)) (mkEquation ti)

-- | Are two expressions equal
--   under a given condition
--   for a given number of tests?
condEqual :: Instances -> Int -> Expr -> Expr -> Expr -> Bool
condEqual ti n pre e1 e2 = maybe False (isTrue $ take n . grounds (lookupTiers ti)) (mkConditionalEquation ti pre e1 e2)

-- | Are two expressions equal
--   under a given condition
--   for a given number of tests
--   and a minimum amount of tests
condEqualM :: Instances -> Int -> Int -> Expr -> Expr -> Expr -> Bool
condEqualM ti n n0 pre e1 e2 = condEqual ti n pre e1 e2 && length cs >= n0
  where
  cs =  fromMaybe []
     $  filter evalBool . map condition . take n . grounds (lookupTiers ti)
    <$> mkConditionalEquation ti pre e1 e2
  condition ceq = let (ce,_,_) = unConditionalEquation ceq in ce

-- | Are two expressions less-than-or-equal for a given number of tests?
lessOrEqual :: Instances -> Int -> Expr -> Expr -> Bool
lessOrEqual ti n = isTrueComparison (take n . grounds (lookupTiers ti)) (mkComparisonLE ti)

-- | Are two expressions less-than for a given number of tests?
less :: Instances -> Int -> Expr -> Expr -> Bool
less ti n = isTrueComparison (take n . grounds (lookupTiers ti)) (mkComparisonLT ti)

-- | Are two expressions inequal for *all* variable assignments?
--   Note this is different than @not . equal@.
inequal :: Instances -> Int -> Expr -> Expr -> Bool
inequal ti n e1 e2 = maybe False (isFalse $ take n . grounds (lookupTiers ti)) (mkEquation ti e1 e2)

-- | Under a maximum number of tests,
--   returns the ratio for which an expression holds true.
trueRatio :: Instances -> Int -> Expr -> Ratio Int
trueRatio is n e = length trueBinds % length gs
  where
  gs = take n $ grounds (lookupTiers is) e
  trueBinds = [e | e <- gs , eval False e]

isTrueComparison :: (Expr -> [Expr]) -> (Expr -> Expr -> Maybe Expr) -> Expr -> Expr -> Bool
isTrueComparison grounds mkComparison e1 e2  =  maybe False (isTrue grounds) (mkComparison e1 e2)

-- | Is a boolean expression true for all variable assignments?
isTrue :: (Expr -> [Expr]) -> Expr -> Bool
isTrue grounds  =  all evalBool . grounds

-- | Is an expression ALWAYS false?
-- This is *NOT* the same as not true.
isFalse :: (Expr -> [Expr]) -> Expr -> Bool
isFalse grounds  =  all (not . evalBool) . grounds

evalBool :: Expr -> Bool
evalBool  =  errorToFalse . eval False
