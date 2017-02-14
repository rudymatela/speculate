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
infixr 0 -==>-

(-&&-) :: Expr -> Expr -> Expr
e1 -&&- e2 = andE :$ e1 :$ e2 where andE = constant "&&" (&&)
infixr 0 -&&-

-- returns a list of errors on the Eq instances (if any)
-- returns an empty list when ok
eqErrors :: Instances -> TypeRep -> Int -> [String]
eqErrors is t n = ["not reflexive"  | f $  x -==- x]
               ++ ["not symmetric"  | f $ (x -==- y) -==- (y -==- x)]
               ++ ["not transitive" | f $  x -==- y -&&- y -==- z -==>- x -==- z]
  where
  f = false is n
  e1 -==- e2 = fromMaybe falseE $ equation is e1 e2
  x = Var "x" t
  y = Var "y" t
  z = Var "z" t

-- returns a list of errors on the Ord instance (if any)
ordErrors :: Instances -> TypeRep -> Int -> [String]
ordErrors is t n = ["not reflexive"     | f $ x -<=- x]
                ++ ["not antisymmetric" | f $ x -<=- y -&&- y -<=- x -==>- x -==- y]
                ++ ["not transitive"    | f $ x -<=- y -&&- y -<=- z -==>- x -<=- z]
  where
  f = false is n
  e1 -==- e2 = fromMaybe falseE $ equation is e1 e2
  e1 -<=- e2 = fromMaybe falseE $ comparisonLE is e1 e2
  x = Var "x" t
  y = Var "y" t
  z = Var "z" t

eqOrdErrors :: Instances -> Int -> TypeRep -> [String]
eqOrdErrors is n t =
     [ "(==) :: " ++ ty ++ "  is not an equiavalence (" ++ intercalate ", " es ++ ")"
     | let es = eqErrors is t n, isEq is t, not (null es) ]
  ++ [ "(<=) :: " ++ ty ++ "  is not an ordering (" ++ intercalate ", " es ++ ")"
     | let es = ordErrors is t n, isOrd is t, not (null es) ]
  ++ [ "(==) and (<=) :: " ++ ty ++ " are inconsistent: (x == y) /= (x <= y && x >= y)"
     | false is n $ (x -==- y) -==- (x -<=- y -&&- y -<=- x)]
  where
  x = Var "x" t
  y = Var "y" t
  z = Var "z" t
  e1 -==- e2 = fromMaybe falseE $ equation is e1 e2
  e1 -<=- e2 = fromMaybe falseE $ comparisonLE is e1 e2
  ty = show t ++ " -> " ++ show t ++ " -> Bool"

instanceErrors :: Instances -> Int -> [TypeRep] -> [String]
instanceErrors is n = concatMap $ eqOrdErrors is n
