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

(-==>-) :: Expr -> Expr -> Expr
e1 -==>- e2 = impliesE :$ e1 :$ e2 where impliesE = constant "==>" (==>)
infixr 0 -==>-

(-&&-) :: Expr -> Expr -> Expr
e1 -&&- e2 = andE :$ e1 :$ e2 where andE = constant "&&" (&&)
infixr 0 -&&-

-- returns a list of errors on the Eq instances (if any)
-- returns an empty list when ok
eqErrors :: Instances -> TypeRep -> Int -> [String]
eqErrors is t n = ["not reflexive"  | tru $  x -==- x]
               ++ ["not symmetric"  | tru $ (x -==- y) -==- (y -==- x)]
               ++ ["not transitive" | tru $  x -==- y -&&- y -==- z -==>- x -==- z]
  where
  tru = true is n
  e1 -==- e2 = fromMaybe falseE $ equation is e1 e2
  x = Var "x" t
  y = Var "y" t
  z = Var "z" t

-- returns a list of errors on the Ord instance (if any)
ordErrors :: Instances -> TypeRep -> Int -> [String]
ordErrors is t n = ["not reflexive"     | tru $ x -<=- x]
                ++ ["not antisymmetric" | tru $ x -<=- y -&&- y -<=- x -==>- x -==- y]
                ++ ["not transitive"    | tru $ x -<=- y -&&- y -<=- z -==>- x -<=- z]
  where
  tru = true is n
  e1 -==- e2 = fromMaybe falseE $ equation is e1 e2
  e1 -<=- e2 = fromMaybe falseE $ comparisonLE is e1 e2
  x = Var "x" t
  y = Var "y" t
  z = Var "z" t

eqOrdErrors :: Instances -> TypeRep -> Int -> [String]
eqOrdErrors is t n = undefined

instanceErrors :: Instances -> Int -> [String]
instanceErrors is n = undefined
