module Test.Speculate.Sanity
  ( instanceErrors
  , eqOrdErrors
  , eqErrors
  , ordErrors
  )
where

import Test.Speculate.Expr

-- NOTE: WARNING: FIXME: TODO: these checks are wrong!!!!!
-- (x == y) == (y == x) for all assignments of x and y
-- NOT:
-- (x == y) for all assignments of x and y
-- equal to
-- (y == x) for all assignments of y and x
-- currently this module implements the last one, not the first

-- returns a list of errors on the Eq instances (if any)
-- returns an empty list when ok
eqErrors :: Instances -> TypeRep -> Int -> [String]
eqErrors is t n = ["not reflexive"  | x === x]
               ++ ["not symmetric"  | (x === y) == (y === x)]
               ++ ["not transitive" | (x === y && y === z ==> x === z)]
  where
  (===) = equal is n
  x = Var "x" t
  y = Var "y" t
  z = Var "z" t

-- returns a list of errors on the Ord instance (if any)
ordErrors :: Instances -> TypeRep -> Int -> [String]
ordErrors is t n = ["not reflexive"     | x <= x]
                ++ ["not antisymmetric" | x <= y && y <= x ==> x == y]
                ++ ["not transitive"    | x <= y && y <= z ==> x <= z]
  where
  (==) = equal is n
  (<=) = lessOrEqual is n
  x = Var "x" t
  y = Var "y" t
  z = Var "z" t

eqOrdErrors :: Instances -> TypeRep -> Int -> [String]
eqOrdErrors is t n = undefined

instanceErrors :: Instances -> Int -> [String]
instanceErrors is t n = undefined
