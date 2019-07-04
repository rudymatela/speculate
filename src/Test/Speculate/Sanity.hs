-- |
-- Module      : Test.Speculate.Sanity
-- Copyright   : (c) 2016-2017 Rudy Matela
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
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Test.Speculate.Utils

(-==>-) :: Expr -> Expr -> Expr
e1 -==>- e2 = impliesE :$ e1 :$ e2 where impliesE = constant "==>" (==>)
infixr 1 -==>-

(-&&-) :: Expr -> Expr -> Expr
e1 -&&- e2 = andE :$ e1 :$ e2 where andE = constant "&&" (&&)
infixr 3 -&&-

-- returns a list of errors on the Eq instances (if any)
-- returns an empty list when ok
eqErrors :: Instances -> Int -> TypeRep -> [String]
eqErrors is n t =
     ["not reflexive"  | f   (x -==- x)]
  ++ ["not symmetric"  | f  ((x -==- y) -==- (y -==- x))]
  ++ ["not transitive" | f (((x -==- y) -&&- (y -==- z)) -==>- (x -==- z))]
  where
  f = not . isTrue is n
  e1 -==- e2 = fromMaybe (val False) $ equation is e1 e2
  e = holeOfTy is t
  x = "x" `varAsTypeOf` e
  y = "y" `varAsTypeOf` e
  z = "z" `varAsTypeOf` e

-- returns a list of errors on the Ord instance (if any)
ordErrors :: Instances -> Int -> TypeRep -> [String]
ordErrors is n t =
     ["not reflexive"     | f   (x -<=- x)]
  ++ ["not antisymmetric" | f (((x -<=- y) -&&- (y -<=- x)) -==>- (x -==- y))]
  ++ ["not transitive"    | f (((x -<=- y) -&&- (y -<=- z)) -==>- (x -<=- z))]
  where
  f = not . isTrue is n
  e1 -==- e2 = fromMaybe (val False) $ equation     is e1 e2
  e1 -<=- e2 = fromMaybe (val False) $ comparisonLE is e1 e2
  e = holeOfTy is t
  x = "x" `varAsTypeOf` e
  y = "y" `varAsTypeOf` e
  z = "z" `varAsTypeOf` e

eqOrdErrors :: Instances -> Int -> TypeRep -> [String]
eqOrdErrors is n t =
     [ "(==) :: " ++ ty ++ "  is not an equiavalence (" ++ intercalate ", " es ++ ")"
     | let es = eqErrors is n t, isEqT is t, not (null es) ]
  ++ [ "(<=) :: " ++ ty ++ "  is not an ordering ("     ++ intercalate ", " es ++ ")"
     | let es = ordErrors is n t, isOrdT is t, not (null es) ]
  ++ [ "(==) and (<=) :: " ++ ty ++ " are inconsistent: (x == y) /= (x <= y && y <= x)"
     | f $ (x -==- y) -==- (x -<=- y -&&- y -<=- x), isEqT is t, isOrdT is t ]
  where
  f = not . isTrue is n
  e = holeOfTy is t
  x = "x" `varAsTypeOf` e
  y = "y" `varAsTypeOf` e
  e1 -==- e2 = fromMaybe (val False) $ equation     is e1 e2
  e1 -<=- e2 = fromMaybe (val False) $ comparisonLE is e1 e2
  ty = show t ++ " -> " ++ show t ++ " -> Bool"

instanceErrors :: Instances -> Int -> [TypeRep] -> [String]
instanceErrors is n = concatMap $ eqOrdErrors is n
