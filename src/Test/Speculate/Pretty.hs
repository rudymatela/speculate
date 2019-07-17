-- |
-- Module      : Test.Speculate.Pretty
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Pretty printing of Equations, Inequalities and Conditional Equations
module Test.Speculate.Pretty
  ( prettyThy, prettyEquations
  , prettyShy, prettySemiEquations
  , prettyChy, prettyCondEquations
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
prettyThy shouldShow ti = prettyEquations . finalEquations shouldShow ti

prettyChy :: (CondEquation -> Bool) -> Chy -> String
prettyChy shouldShow = prettyCondEquations . finalCondEquations shouldShow

prettyShy :: (Equation -> Bool) -> Instances -> (Expr -> Expr -> Bool) -> Shy -> String
prettyShy shouldShow insts equivalentInstanceOf =
  prettySemiEquations . finalSemiEquations shouldShow insts equivalentInstanceOf

prettyEquations :: [Equation] -> String
prettyEquations =
  table "r l l" . map showEquation
  where
  showEquation (e1,e2)
--  | typ e1 == boolTy = [showOpExpr "<==>" e1, "<==>", showOpExpr "<==>" e2]
    | otherwise        = [showOpExpr "==" e1, "==", showOpExpr "==" e2]

prettySemiEquations :: [Equation] -> String
prettySemiEquations =
  table "r l l" . map showSELine
  where
  showSELine (e1,e2) = showLineWithOp (if typ e1 == boolTy then "==>" else "<=") (e1,e2)
  showLineWithOp o (e1,e2) = [showOpExpr o e1, o, showOpExpr o e2]

prettyCondEquations :: [CondEquation] -> String
prettyCondEquations =
  table "r r r l l" . map showCELine
  where
  showCELine (ce,e1,e2) = [ showOpExpr "==>" ce
                          , "==>", showOpExpr "==" e1
                          , "==",  showOpExpr "==" e2 ]
