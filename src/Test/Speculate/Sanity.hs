-- |
-- Module      : Test.Speculate.Sanity
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Sanity checks for before running the Speculate algorithm.
module Test.Speculate.Sanity
  ( instanceErrors
  , eqOrdErrors
  , eqErrors
  , ordErrors
  )
where

import Test.Speculate.Expr
import Test.LeanCheck ((==>))
import Data.List (intercalate)

(-==>-) :: Expr -> Expr -> Expr
e1 -==>- e2 = impliesE :$ e1 :$ e2 where impliesE = value "==>" (==>)
infixr 1 -==>-

(-&&-) :: Expr -> Expr -> Expr
e1 -&&- e2 = andE :$ e1 :$ e2 where andE = value "&&" (&&)
infixr 3 -&&-

-- returns a list of errors on the Eq instances (if any)
-- returns an empty list when ok
eqErrors :: Instances -> Int -> TypeRep -> [String]
eqErrors is n t = takeWhile (const $ isListableT is t) $
     ["not reflexive"  | f   (x -==- x)]
  ++ ["not symmetric"  | f  ((x -==- y) -==- (y -==- x))]
  ++ ["not transitive" | f (((x -==- y) -&&- (y -==- z)) -==>- (x -==- z))]
  where
  f = not . isTrue (take n . grounds (lookupTiers is))
  e1 -==- e2 = mkEquation is e1 e2
  e = holeOfTy is t
  x = "x" `varAsTypeOf` e
  y = "y" `varAsTypeOf` e
  z = "z" `varAsTypeOf` e

-- returns a list of errors on the Ord instance (if any)
ordErrors :: Instances -> Int -> TypeRep -> [String]
ordErrors is n t = takeWhile (const $ isListableT is t) $
     ["not reflexive"     | f   (x -<=- x)]
  ++ ["not antisymmetric" | f (((x -<=- y) -&&- (y -<=- x)) -==>- (x -==- y))]
  ++ ["not transitive"    | f (((x -<=- y) -&&- (y -<=- z)) -==>- (x -<=- z))]
  where
  f = not . isTrue (take n . grounds (lookupTiers is))
  e1 -==- e2 = mkEquation     is e1 e2
  e1 -<=- e2 = mkComparisonLE is e1 e2
  e = holeOfTy is t
  x = "x" `varAsTypeOf` e
  y = "y" `varAsTypeOf` e
  z = "z" `varAsTypeOf` e

eqOrdErrors :: Instances -> Int -> TypeRep -> [String]
eqOrdErrors is n t =
     [ "(==) :: " ++ ty ++ "  is not an equiavalence (" ++ intercalate ", " es ++ ")"
     | listable, eq,  let es = eqErrors  is n t, not (null es) ]
  ++ [ "(<=) :: " ++ ty ++ "  is not an ordering ("     ++ intercalate ", " es ++ ")"
     | listable, ord, let es = ordErrors is n t, not (null es) ]
  ++ [ "(==) and (<=) :: " ++ ty ++ " are inconsistent: (x == y) /= (x <= y && y <= x)"
     | listable, eq, ord, f $ (x -==- y) -==- (x -<=- y -&&- y -<=- x) ]
  where
  listable = isListableT is t
  eq       = isEqT       is t
  ord      = isOrdT      is t
  f = not . isTrue (take n . grounds (lookupTiers is))
  e = holeOfTy is t
  x = "x" `varAsTypeOf` e
  y = "y" `varAsTypeOf` e
  e1 -==- e2 = mkEquation is e1 e2
  e1 -<=- e2 = mkComparisonLE is e1 e2
  ty = show t ++ " -> " ++ show t ++ " -> Bool"

instanceErrors :: Instances -> Int -> [TypeRep] -> [String]
instanceErrors is n = concatMap $ eqOrdErrors is n
