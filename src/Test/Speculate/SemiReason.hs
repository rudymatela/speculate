-- |
-- Module      : Test.Speculate.Misc
-- Copyright   : (c) 2016-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part o Speculate.
--
-- Inequational (or semi-equational) reasoning.
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
  , sthy :: Thy
  }

emptyShy :: Shy
emptyShy = Shy
  { sequations = []
  , sthy = emptyThy
  }

updateSemiEquationsBy :: ([Equation] -> [Equation]) -> Shy -> Shy
updateSemiEquationsBy f shy@Shy {sequations = es} = shy {sequations = f es}

mapSemiEquations :: (Equation -> Equation) -> Shy -> Shy
mapSemiEquations = updateSemiEquationsBy . map

scompareE :: Shy -> (Expr -> Expr -> Ordering)
scompareE = compareE . sthy

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
  isSEInstanceOf = isInstanceOf `on` foldPair
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

stheorize :: Thy -> [Equation] -> Shy
stheorize thy seqs =
  Shy{ sequations = sortBy (compareE thy `on` foldPair) seqs
     , sthy = thy
     }

-- list all equation sides in a Shy
sides :: Shy -> [Expr]
sides shy = nubSortBy (scompareE shy)
          . concatMap (\(e1,e2) -> [e1,e2])
          $ sequations shy

finalSemiEquations :: (Equation -> Bool) -> Instances -> (Expr -> Expr -> Bool) -> Shy -> [Equation]
finalSemiEquations shouldShow insts equivalentInstanceOf shy =
    sortBy (compareTy `on` (typ . fst))
  . filter shouldShow
  . discardLater (equivalentInstanceOf `on` foldPair)
  . discard (transConsequence shy)
  . discardLater (isInstanceOf `on` foldPair)
  . sequations
  $ canonicalizeShyWith insts shy

canonicalizeShyWith :: Instances -> Shy -> Shy
canonicalizeShyWith = mapSemiEquations . canonicalizeSemiEquationWith

canonicalizeSemiEquationWith :: Instances -> Equation -> Equation
canonicalizeSemiEquationWith is (e1,e2) =
  case canonicalizeWith (lookupNames is) (e1 :$ e2) of
  e1' :$ e2' -> (e1',e2')
  _ -> error $ "canonicalizeShyWith: the impossible happened,"
            ++ "this is definitely a bug, see source!"
