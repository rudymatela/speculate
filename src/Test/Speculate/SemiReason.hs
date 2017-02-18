module Test.Speculate.SemiReason where

import Test.Speculate.Expr
import Test.Speculate.Reason
import Test.Speculate.Utils
import Data.List as L (sortBy, delete)
import Data.Function (on)

type Equation = (Expr, Expr)
-- Maybe (Bool, Expr, Expr)?  where bool tells if it is strict

data Shy = Shy
  { sequations  :: [Equation] -- <='s
--, ssequations :: [Equation] -- <'s -- LATER!
  }

emptyShy = Shy
  { sequations = []
  }

lesser  :: Shy -> Expr -> [Expr]
lesser  shy e = [ e1 | (e1,e2) <- sequations shy, e == e2 ]

greater :: Shy -> Expr -> [Expr]
greater shy e = [ e2 | (e1,e2) <- sequations shy, e == e1 ]

-- | given a semi-equation (inequality),
--   simplerThan restricts the Shy (SemiTheory)
--   into only equations simpler
--   than the given semi-equation
--   or that are instances of simpler equations.
--
-- half-baked example:
--
-- @x + 1@ is simpler than @x + y@ and it is returned.
-- @(1 + 1) + 1@ is more complex than @x + y@
-- but it is returned as well as it is an instance of @x + 1@.
simplerThan :: Equation -> Shy -> Shy
simplerThan seq = updateSEquationsBy upd
  where
  isSEInstanceOf = isInstanceOf `on` uncurry phonyEquation
  upd eqs = r ++ [seq' | seq' <- r'
                       , any (seq' `isSEInstanceOf`) r ]
    where
    r  =          takeWhile (/= seq) eqs
    r' = drop 1 $ dropWhile (/= seq) eqs
-- simplerThan used to be just:
-- simplerThan seq = updateSEquationsBy (takeWhile (/= seq))

transConsequence :: Shy -> Equation -> Bool
transConsequence shy (e1,e2) = or [ e1' == e2'
                                  | e1' <- L.delete e2 $ greater shy' e1
                                  , e2' <- L.delete e1 $ lesser  shy' e2
                                  ]
  where
  shy' = simplerThan (e1,e2) shy

updateSEquationsBy :: ([Equation] -> [Equation]) -> Shy -> Shy
updateSEquationsBy f shy@Shy{sequations = seqs} = shy{sequations = f seqs}

stheorize :: [Equation] -> Shy
stheorize seqs =
  Shy{sequations = sortBy (compareComplexity `on` uncurry phonyEquation) seqs}

prettyShy :: (Equation -> Bool) -> (Expr -> Expr -> Bool) -> Shy -> String
prettyShy shouldShow equivalentInstanceOf shy =
    table "r l l"
  . map showSELine
  . sortOn (typ . fst)
  . filter shouldShow
  . discardLater (equivalentInstanceOf `on` uncurry phonyEquation)
  . discard (transConsequence shy)
  . discardLater (isInstanceOf `on` uncurry phonyEquation)
  $ sequations shy
  where
  showSELine (e1,e2) = showLineWithOp (if typ e1 == boolTy then "==>" else "<=") (e1,e2)
  showLineWithOp o (e1,e2) = [showOpExpr o e1, o, showOpExpr o e2]
