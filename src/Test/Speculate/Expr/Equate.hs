-- | This module exports
--     smart constructors,
--     smart destructors
--   and queries over
--     equations,
--     inequations
--     and conditional equations.
module Test.Speculate.Expr.Equate
  ( equation, unEquation, isEquation, uselessEquation, usefulEquation
  , phonyEquation

  , comparison, comparisonL, comparisonLE, unComparison

  , implication, unImplication, usefulImplication

  , conditionalEquation, unConditionalEquation, usefulConditionalEquation
  , conditionalComparisonL, conditionalComparisonLE, unConditionalComparison
  )
where

import Test.LeanCheck ((==>))
import Data.List ((\\))
import Test.Speculate.Utils
import Test.Speculate.Expr.Core
import Test.Speculate.Expr.TypeInfo

equation :: TypeInfo -> Expr -> Expr -> Maybe Expr
equation ti e1 e2 = do
  eqE <- equalityE ti (typ e1)
  eqE :$ e1 $$ e2

phonyEquation :: Expr -> Expr -> Expr
phonyEquation e1 e2 | typ e1 /= typ e2 = error $ "phonyEquation: type mismatch "
                                              ++ show (typ e1) ++ ", "
                                              ++ show (typ e2)
phonyEquation e1 e2 = Var "==" (mkEqnTy $ typ e1) :$ e1 :$ e2

unEquation :: Expr -> (Expr,Expr)
unEquation (((Constant "==" _) :$ e1) :$ e2) = (e1,e2)
unEquation (((Var      "==" _) :$ e1) :$ e2) = (e1,e2)
unEquation _ = error "unEquation: not an equation!"

isEquation :: Expr -> Bool
isEquation (((Constant "==" _) :$ e1) :$ e2) = True
isEquation (((Var      "==" _) :$ e1) :$ e2) = True
isEquation _ = False

-- | Given an equation encoded as an 'Expr'.
--   Checks if both sides of an equation are the same.
--   If the 'Expr' is not an equation, this raises an error.
uselessEquation :: Expr -> Bool
uselessEquation = uncurry (==) . unEquation

usefulEquation :: Expr -> Bool
usefulEquation = uncurry (/=) . unEquation

comparison :: TypeInfo -> Expr -> Expr -> Maybe Expr
comparison ti e1 e2 = do
  ti <- findInfo (typ e1) ti
  compareE1 ti :$ e1 $$ e2

comparisonL :: TypeInfo -> Expr -> Expr -> Maybe Expr
comparisonL ti e1 e2 = do
  e <- lessE ti (typ e1)
  e :$ e1 $$ e2

comparisonLE :: TypeInfo -> Expr -> Expr -> Maybe Expr
comparisonLE ti e1 e2 = do
  e <- lessEqE ti (typ e1)
  e :$ e1 $$ e2

unComparison :: Expr -> (Expr,Expr)
unComparison (((Constant "compare"  _) :$ e1) :$ e2) = (e1,e2)
unComparison (((Constant "<"        _) :$ e1) :$ e2) = (e1,e2)
unComparison (((Constant "<="       _) :$ e1) :$ e2) = (e1,e2)
unComparison (((Constant ">"        _) :$ e1) :$ e2) = (e1,e2)
unComparison (((Constant ">="       _) :$ e1) :$ e2) = (e1,e2)
unComparison _ = error "unComparisonL: not a compare/(<)/(<=)/(>)/(>=) application"

implication :: Expr -> Expr -> Maybe Expr
implication e1 e2
  | typ e1 == boolTy = implicationE :$ e1 $$ e2
  | otherwise        = Nothing
  where
  implicationE = constant "==>" (==>)

unImplication :: Expr -> (Expr,Expr)
unImplication (((Constant "==>" _) :$ e1) :$ e2) = (e1,e2)
unImplication _ = error "unImplication: not an implication"

usefulImplication :: Expr -> Bool
usefulImplication e = vp \\ ve /= vp
  where
  (pre,e') = unImplication e
  vp = vars pre
  ve = vars e'

conditionalEquation :: TypeInfo -> Expr -> Expr -> Expr -> Maybe Expr
conditionalEquation ti pre e1 e2 = (pre `implication`) =<< equation ti e1 e2

unConditionalEquation :: Expr -> (Expr,Expr,Expr)
unConditionalEquation (((Constant "==>" _) :$ pre) :$ (((Constant "==" _) :$ e1) :$ e2)) = (pre,e1,e2)
unConditionalEquation _ = error "unConditionalEquation: not an equation with side condition"

-- an equation with a side condition is useful when sides of the equation are different
-- and at least one variable is shared between the side condition and the equation
usefulConditionalEquation :: Expr -> Bool
usefulConditionalEquation e = e1 /= e2 && vp \\ ve /= vp
  where
  (pre,e1,e2) = unConditionalEquation e
  vp = vars pre
  ve = vars e1 +++ vars e2

conditionalComparisonLE :: TypeInfo -> Expr -> Expr -> Expr -> Maybe Expr
conditionalComparisonLE ti pre e1 e2 = (pre `implication`) =<< comparisonLE ti e1 e2

conditionalComparisonL :: TypeInfo -> Expr -> Expr -> Expr -> Maybe Expr
conditionalComparisonL ti pre e1 e2 = (pre `implication`) =<< comparisonL ti e1 e2

unConditionalComparison :: Expr -> (Expr,Expr,Expr)
unConditionalComparison e = (econd,e1,e2)
  where
  (e1,e2) = unComparison ecmp
  (econd,ecmp) = unImplication e
