-- |
-- Module      : Test.Speculate.Expr.Ground
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Generate and evaluate ground values of expressions.
module Test.Speculate.Expr.Ground
  ( grounds
  , groundBinds
  , groundAndBinds
  , equal
  , lessOrEqual
  , less
  , inequal
  , isTrue
  , isFalse
  , condEqual
  , condEqualM
  , trueBinds
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

-- TODO: move vassignments / etc here

-- | List all possible valuations of an expression (potentially infinite).
--   In pseudo-Haskell:
--
-- > take 3 $ grounds preludeInstances ((x + x) + y)
-- >   == [(0 + 0) + 0, (0 + 0) + 1, (1 + 1) + 0]
grounds :: Instances -> Expr -> [Expr]
grounds ti e = (e //-) <$> groundBinds ti e

-- | List all possible variable bindings to an expression
--
-- > take 3 $ groundBinds preludeInstances ((x + x) + y)
-- >   == [ [("x",0),("y",0)]
-- >      , [("x",0),("y",1)]
-- >      , [("x",1),("y",0)] ]
groundBinds :: Instances -> Expr -> [Binds]
groundBinds ti e =
  concat $ products [mapT ((,) v) (tiersE ti (typ v)) | v <- nubVars e]

-- | List all possible variable bindings and valuations to an expression
--
-- > groundAndBinds ti e == zipWith (,) (grounds ti e) (groundBinds ti e)
groundAndBinds :: Instances -> Expr -> [(Binds,Expr)]
groundAndBinds ti e = (\bs -> (bs, e //- bs)) <$> groundBinds ti e


-- | Are two expressions equal for a given number of tests?
equal :: Instances -> Int -> Expr -> Expr -> Bool
-- equal ti _ e1 e2 | e1 == e2 = isComparable ti e1 -- optional optimization
equal ti n e1 e2 = case mkEquation ti e1 e2 of
  Just eq | all (isListable ti . typ) (nubVars eq) -> isTrue ti n eq
  _                                                -> False
-- TODO: discover why the optimization above changes the output
-- 1. $ make eg/list && ./eg/list -ES -r0 -s4 > without
-- 2. uncomment above
-- 3. $ make eg/list && ./eg/list -ES -r0 -s4 > with
-- 4. diff -rud without with
-- 5. see that there are less equivalence classes now!

-- | Are two expressions equal
--   under a given condition
--   for a given number of tests?
condEqual :: Instances -> Int -> Expr -> Expr -> Expr -> Bool
condEqual ti n pre e1 e2 = maybe False (isTrue ti n) (mkConditionalEquation ti pre e1 e2)

-- | Are two expressions equal
--   under a given condition
--   for a given number of tests
--   and a minimum amount of tests
condEqualM :: Instances -> Int -> Int -> Expr -> Expr -> Expr -> Bool
condEqualM ti n n0 pre e1 e2 = condEqual ti n pre e1 e2 && length cs >= n0
  where
  cs =  fromMaybe []
     $  filter evalBool . map condition . take n . grounds ti
    <$> mkConditionalEquation ti pre e1 e2
  condition ceq = let (ce,_,_) = unConditionalEquation ceq in ce

-- | Are two expressions less-than-or-equal for a given number of tests?
lessOrEqual :: Instances -> Int -> Expr -> Expr -> Bool
lessOrEqual ti n e1 e2 = maybe False (isTrue ti n) (mkComparisonLE ti e1 e2)

-- | Are two expressions less-than for a given number of tests?
less        :: Instances -> Int -> Expr -> Expr -> Bool
less        ti n e1 e2 = maybe False (isTrue ti n) (mkComparisonLT ti e1 e2)

-- | Are two expressions inequal for *all* variable assignments?
--   Note this is different than @not . equal@.
inequal :: Instances -> Int -> Expr -> Expr -> Bool
inequal ti n e1 e2 = maybe False (isFalse ti n) (mkEquation ti e1 e2)

-- | Is a boolean expression true for all variable assignments?
isTrue :: Instances -> Int -> Expr -> Bool
isTrue ti n e = all evalBool . take n $ grounds ti e

-- | List variable bindings for which an expression holds true.
trueBinds :: Instances -> Int -> Expr -> [Binds]
trueBinds ti n e = [bs | (bs,e) <- take n $ groundAndBinds ti e, evalBool e]

-- | Under a maximum number of tests,
--   returns the ratio for which an expression holds true.
trueRatio :: Instances -> Int -> Expr -> Ratio Int
trueRatio ti n e = length (trueBinds ti n e) % length (take n $ groundAndBinds ti e)

-- | Is an expression ALWAYS false?
-- This is *NOT* the same as not true.
isFalse :: Instances -> Int -> Expr -> Bool
isFalse ti n e = all (not . evalBool) . take n $ grounds ti e

evalBool :: Expr -> Bool
evalBool  =  errorToFalse . eval False
