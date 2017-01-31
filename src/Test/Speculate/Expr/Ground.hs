module Test.Speculate.Expr.Ground
  ( grounds
  , groundBinds
  , groundAndBinds
  , equal
  , lessOrEqual
  , less
  , inequal
  , true
  , false
  , condEqual
  , trueBinds
  , trueRatio
  )
where

import Test.Speculate.Expr.Core
import Test.Speculate.Expr.Match
import Test.Speculate.Expr.TypeInfo
import Test.Speculate.Expr.Equate
import Test.LeanCheck
import Data.Ratio
import Data.Functor ((<$>)) -- for GHC < 7.10

-- TODO: move vassignments / etc here

-- | List all possible valuations of an expression (potentially infinite).
--   In pseudo-Haskell:
--
-- > take 3 $ grounds basicTypeInfo ((x + x) + y)
-- >   == [(0 + 0) + 0, (0 + 0) + 1, (1 + 1) + 0]
grounds :: TypeInfo -> Expr -> [Expr]
grounds ti e = (e `assigning`) <$> groundBinds ti e

-- | List all possible variable bindings to an expression
--
-- > take 3 $ groundBinds basicTypeInfo ((x + x) + y)
-- >   == [ [("x",0),("y",0)]
-- >      , [("x",0),("y",1)]
-- >      , [("x",1),("y",0)] ]
groundBinds :: TypeInfo -> Expr -> [Binds]
groundBinds ti e =
  concat $ products [mapT ((,) n) ts | (t,n) <- vars e, let ts = tiersE t ti]

-- | List all possible variable bindings and valuations to an expression
--
-- > groundAndBinds ti e == zipWith (,) (grounds ti e) (groundBinds ti e)
groundAndBinds :: TypeInfo -> Expr -> [(Binds,Expr)]
groundAndBinds ti e = (\bs -> (bs, e `assigning` bs)) <$> groundBinds ti e

-- | Are two expressions equal for a given number of tests?
equal :: TypeInfo -> Int -> Expr -> Expr -> Bool
-- equal ti _ e1 e2 | e1 == e2 = isComparable ti e1 -- optional optimization
equal ti n e1 e2 = maybe False (true ti n) (equation ti e1 e2)
-- TODO: discover why the optimization above changes the output
-- 1. $ make eg/list && ./eg/list -ES -r0 -s4 > without
-- 2. uncomment above
-- 3. $ make eg/list && ./eg/list -ES -r0 -s4 > with
-- 4. diff -rud without with
-- 5. see that there are less equivalence classes now!

-- | Are two expressions equal
--   under a given condition
--   for a given number of tests?
condEqual :: TypeInfo -> Int -> Expr -> Expr -> Expr -> Bool
condEqual ti n pre e1 e2 = maybe False (true ti n) (conditionalEquation ti pre e1 e2)

-- | Are two expressions less-than-or-equal for a given number of tests?
lessOrEqual :: TypeInfo -> Int -> Expr -> Expr -> Bool
lessOrEqual ti n e1 e2 = maybe False (true ti n) (comparisonLE ti e1 e2)

-- | Are two expressions less-than for a given number of tests?
less        :: TypeInfo -> Int -> Expr -> Expr -> Bool
less        ti n e1 e2 = maybe False (true ti n) (comparisonL  ti e1 e2)

-- | Are two expressions inequal for *all* variable assignments?
--   Note this is different than @not . equal@.
inequal :: TypeInfo -> Int -> Expr -> Expr -> Bool
inequal ti n e1 e2 = maybe False (false ti n) (equation ti e1 e2)

-- | Is a boolean expression true for all variable assignments?
true :: TypeInfo -> Int -> Expr -> Bool
true ti n e = and . map (eval False) . take n $ grounds ti e

-- | List variable bindings for which an expression holds true.
trueBinds :: TypeInfo -> Int -> Expr -> [Binds]
trueBinds ti n e = [bs | (bs,e) <- take n $ groundAndBinds ti e, eval False e == True]

-- | Under a maximum number of tests,
--   returns the ratio for which an expression holds true.
trueRatio :: TypeInfo -> Int -> Expr -> Ratio Int
trueRatio ti n e = length (trueBinds ti n e) % length (take n $ groundAndBinds ti e)

-- | Is an expression ALWAYS false?
false :: TypeInfo -> Int -> Expr -> Bool
false ti n e = and . map (not . (eval False)) . take n $ grounds ti e
