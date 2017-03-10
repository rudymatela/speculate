-- | Pretty printing of Equations, Inequalities and Conditional Equations
module Test.Speculate.Pretty
  ( prettyThy
  , prettyShy
  , prettyChy
  )
where

import Test.Speculate.Expr
import Test.Speculate.Utils.PrettyPrint
import Test.Speculate.Reason     (Thy, finalEquations)
import Test.Speculate.SemiReason (Shy, finalSemiEquations)
import Test.Speculate.CondReason (Chy, finalCondEquations)

type Equation = (Expr,Expr)
type CondEquation = (Expr,Expr,Expr)

prettyThy :: (Equation -> Bool) -> Instances -> Thy -> String
prettyThy shouldShow ti =
  table "r l l" . map showEquation . finalEquations shouldShow ti
  where
  showEquation (e1,e2)
--  | typ e1 == boolTy = [showOpExpr "<==>" e1, "<==>", showOpExpr "<==>" e2]
    | otherwise        = [showOpExpr "==" e1, "==", showOpExpr "==" e2]

prettyChy :: (CondEquation -> Bool) -> Chy -> String
prettyChy shouldShow =
    table "r r r l l"
  . map (\(pre,e1,e2) -> [ showOpExpr "==>" pre
                         , "==>", showOpExpr "==" e1
                         , "==",  showOpExpr "==" e2 ])
  . finalCondEquations shouldShow

prettyShy :: (Equation -> Bool) -> Instances -> (Expr -> Expr -> Bool) -> Shy -> String
prettyShy shouldShow insts equivalentInstanceOf =
  table "r l l" . map showSELine . finalSemiEquations shouldShow insts equivalentInstanceOf
  where
  showSELine (e1,e2) = showLineWithOp (if typ e1 == boolTy then "==>" else "<=") (e1,e2)
  showLineWithOp o (e1,e2) = [showOpExpr o e1, o, showOpExpr o e2]
