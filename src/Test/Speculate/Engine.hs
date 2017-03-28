-- |
-- Module      : Test.Speculate.Engine
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Main engine to process data.
module Test.Speculate.Engine
  ( vassignments
  , expansions
  , mostGeneral
  , mostSpecific

  , theoryAndRepresentativesFromAtoms
  , theoryFromAtoms
  , equivalencesBetween

  , consider
  , distinctFromSchemas
  , classesFromSchemas

  , semiTheoryFromThyAndReps

  , conditionalTheoryFromThyAndReps
  , conditionalEquivalences
  , subConsequence

  , psortBy

  , module Test.Speculate.Expr
  )
where

import Data.Dynamic
import Data.Maybe
import Data.List hiding (insert)
import Data.Function (on)
import Data.Monoid ((<>))

import Test.Speculate.Utils
import Test.Speculate.Expr
import Test.Speculate.Reason
import Test.Speculate.CondReason
import Test.Speculate.SemiReason
import Test.Speculate.Utils.Class (Class)
import qualified Test.Speculate.Utils.Class as C
import qualified Test.Speculate.Utils.Digraph as D

------------------------------
-- * Manipulating expressions

-- | List all relevant variable assignments in an expresssion.
--   In pseudo-Haskell:
--
-- > vassignments (0 + x) == [0 + x]
-- > vassignments (0 + 0) == [0 + 0]
-- > vassignments (0 + _) == [0 + x]
-- > vassignments (_ + _) == [x + x, x + y]
-- > vassignments (_ + (_ + ord _)) == [x + (x + ord c), x + (y + ord c)]
--
-- You should not use this on expression with already assinged variables
-- (undefined, but currently defined behavior):
--
-- > vassignments (ii -+- i_) == [ii -+- ii]
vassignments :: Expr -> [Expr]
vassignments e =
  [ foldl fill e [ [ Var (defNames !! i) t | i <- is ]
                 | (t,is) <- fs ]
  | fs <- productsList [[(t,is) | is <- iss 0 c] | (t,c) <- counts (holes e)] ]
  -- > fss _ + _ = [ [(Int,[0,0])], [(Int,[0,1])] ]
  -- > fss _ + (_ + ord _) = [ [(Int,[0,0]),(Char,[1])]
  -- >                       , [(Int,[0,1]),(Char,[1])] ]
-- TODO: rename vassignments, silly name.  what about canonicalExpansions?

vassignmentsEqn :: (Expr,Expr) -> [(Expr,Expr)]
vassignmentsEqn = filter (uncurry (/=)) . map unEquation . vassignments . uncurry phonyEquation

expansions :: Instances -> Int -> Expr -> [Expr]
expansions ti n e =
  [ foldl fill e [ [ Var (names ti t !! i) t | i <- is ]
                 | (t,is) <- fs ]
  | fs <- productsList [[(t,is) | is <- foo c n] | (t,c) <- counts (holes e)] ]
  where
  foo :: Int -> Int -> [[Int]]
  foo 0 nVars = [[]]
  foo nPos nVars = [i:is | i <- [0..(nVars-1)], is <- foo (nPos-1) nVars]
-- TODO: test expansions, put foo together with iss

-- | List the most general assignment of holes in an expression
mostGeneral :: Expr -> Expr
mostGeneral = head . vassignments -- TODO: make this efficient

-- | List the most specific assignment of holes in an expression
mostSpecific :: Expr -> Expr
mostSpecific = last . vassignments -- TODO: make this efficient

rehole :: Expr -> Expr
rehole (e1 :$ e2) = rehole e1 :$ rehole e2
rehole (Var _ t) = Var "" t
rehole e = e

----------------------------
-- * Enumerating expressions

-- | Computes a theory from atomic expressions.  Example:
--
-- > > theoryFromAtoms 5 compare (const True) (equal preludeInstances 100)
-- > >   [hole (undefined :: Int),constant "+" ((+) :: Int -> Int -> Int)]
-- > Thy { rules = [ (x + y) + z == x + (y + z) ]
-- >     , equations = [ y + x == x + y
-- >                   , y + (x + z) == x + (y + z)
-- >                   , z + (x + y) == x + (y + z)
-- >                   , z + (y + x) == x + (y + z) ]
-- >     , canReduceTo = (|>)
-- >     , closureLimit = 2
-- >     , keepE = keepUpToLength 5
-- >     }
theoryFromAtoms :: Int -> (Expr -> Expr -> Ordering) -> (Expr -> Bool) -> (Expr -> Expr -> Bool) -> [Expr] -> Thy
theoryFromAtoms sz cmp keep (===) = fst . theoryAndRepresentativesFromAtoms sz cmp keep (===)

representativesFromAtoms :: Int -> (Expr -> Expr -> Ordering) -> (Expr -> Bool) -> (Expr -> Expr -> Bool) -> [Expr] -> [Expr]
representativesFromAtoms sz cmp keep (===) = snd . theoryAndRepresentativesFromAtoms sz cmp keep (===)

expand :: (Expr -> Bool) -> (Expr -> Expr -> Bool) -> (Thy,[Expr]) -> (Thy,[Expr])
expand keep (===) (thy,ss) = foldl (flip $ consider (===)) (thy,ss)
                           . concat . zipWithReverse (*$*)
                           $ collectOn lengthE ss
  where
  fes *$* xes = filter keep $ catMaybes [fe $$ xe | fe <- fes, xe <- xes]

-- | Given atomic expressions, compute theory and representative schema
--   expressions.
theoryAndRepresentativesFromAtoms :: Int
                                  -> (Expr -> Expr -> Ordering)
                                  -> (Expr -> Bool) -> (Expr -> Expr -> Bool)
                                  -> [Expr] -> (Thy,[Expr])
theoryAndRepresentativesFromAtoms sz cmp keep (===) ds =
  iterate ((complete *** id) . expand keep (===)) dsThy !! (sz-1)
  where
  dsThy = (complete *** id) $ foldl (flip $ consider (===)) (iniThy,[]) ds
  iniThy = emptyThy { keepE = keepUpToLength sz
                    , closureLimit = 2
                    , canReduceTo = dwoBy (\e1 e2 -> e1 `cmp` e2 == GT)
                    , compareE = cmp
                    }

-- considers a schema
consider :: (Expr -> Expr -> Bool) -> Expr -> (Thy,[Expr]) -> (Thy,[Expr])
consider (===) s (thy,ss)
  | not (s === s) = (thy,ss++[s])  -- uncomparable type
  | rehole (normalizeE thy (mostGeneral s)) `elem` ss = (thy,ss)
  | otherwise =
    ( append thy $ equivalencesBetween (===) s s ++ eqs
    , ss ++ [s | not $ any (\(e1,e2) -> unrepeatedVars e1 && unrepeatedVars e2) eqs])
    where
    eqs = concatMap (equivalencesBetween (===) s) $ filter (s ===) ss

distinctFromSchemas :: Instances -> Int -> Int -> Thy -> [Expr] -> [Expr]
distinctFromSchemas ti nt nv thy = map C.rep . classesFromSchemas ti nt nv thy

-- > > classesFromSchemas preludeInstances 500 2 thy [_ + _, _ + (_ + _)]
-- > [ (x + x :: Int,[])
-- > , (x + y :: Int,[y + x :: Int])
-- > , (y + y :: Int,[])
-- > , (x + (x + x) :: Int,[])
-- > , (x + (x + y) :: Int,[x + (y + x) :: Int,y + (x + x) :: Int])
-- > , (x + (y + y) :: Int,[y + (x + y) :: Int,y + (y + x) :: Int])
-- > , (y + (y + y) :: Int,[]) ]
classesFromSchemas :: Instances -> Int -> Int -> Thy -> [Expr] -> [Class Expr]
classesFromSchemas ti nt nv thy = C.mergesThat (equal ti nt)
                                . C.mergesOn (normalizeE thy)
                                . concatMap (classesFromSchema ti thy nv)
-- the "mergesThat (equal ...)" above is necesary because "equivalent thy"
-- won't detect all equivalences.  here we test the few remaining
-- there shouldn't be that much overhead

-- | Returns all classes of expressions that can be build from expression
--   schemas (single variable expressions).  Examples:
--
-- > > classesFromSchema preludeInstances thy 2 (i_ -+- i_)
-- > [ (x + x :: Int,[])
-- > , (x + y :: Int,[])
-- > , (y + x :: Int,[])
-- > , (y + y :: Int,[]) ]
classesFromSchema :: Instances -> Thy -> Int -> Expr -> [Class Expr]
classesFromSchema ti thy n = C.mergesOn (normalizeE thy)
                           . map C.fromRep
                           . expansions ti n

-- Return relevant equivalences between holed expressions:
--
-- > equivalencesBetween basicInstances 500 (_ + _) (_ + _) =
-- >   [i + j == j + i]
equivalencesBetween :: (Expr -> Expr -> Bool) -> Expr -> Expr -> [(Expr,Expr)]
equivalencesBetween (===) e1 e2 = discardLater (isInstanceOf `on` uncurry phonyEquation)
                                . filter (uncurry (===))
                                $ vassignmentsEqn (e1,e2)

semiTheoryFromThyAndReps :: Instances -> Int -> Int
                         -> Thy -> [Expr] -> Shy
semiTheoryFromThyAndReps ti nt nv thy =
    stheorize thy
  . pairsThat (\e1 e2 -> e1 /= e2
                      && typ e1 == typ e2
                      && lessOrEqual ti nt e1 e2)
  . distinctFromSchemas ti nt nv thy
  . filter (isOrdE ti)

conditionalTheoryFromThyAndReps :: Instances
                                -> (Expr -> Expr -> Ordering)
                                -> Int -> Int -> Int
                                -> Thy -> [Expr] -> Chy
conditionalTheoryFromThyAndReps ti cmp nt nv csz thy es' =
  conditionalEquivalences
    cmp
    (canonicalCEqnBy cmp ti)
    (condEqual ti nt)
    (lessOrEqual ti nt)
    csz thy clpres cles
  where
  (cles,clpres) = (id *** filter (\(e,_) -> lengthE e <= csz))
                . partition (\(e,_) -> typ e /= boolTy)
                . filter (isEqE ti . fst)
                $ classesFromSchemas ti nt nv thy es'

conditionalEquivalences :: (Expr -> Expr -> Ordering)
                        -> ((Expr,Expr,Expr) -> Bool)
                        -> (Expr -> Expr -> Expr -> Bool)
                        -> (Expr -> Expr -> Bool)
                        -> Int -> Thy -> [Class Expr] -> [Class Expr] -> Chy
conditionalEquivalences cmp canon cequal (==>) csz thy clpres cles =
    cdiscard (\(ce,e1,e2) -> subConsequence thy clpres ce e1 e2)
  . foldl (flip cinsert) (Chy [] cdg clpres thy)
  . sortBy (\(c1,e11,e12) (c2,e21,e22) -> c1 `cmp` c2
                                       <> ((e11 `phonyEquation` e12) `cmp` (e21 `phonyEquation` e22)))
  . discard (\(pre,e1,e2) -> pre == falseE
                          || length (vars pre \\ (vars e1 +++ vars e2)) > 0
                          || subConsequence thy [] pre e1 e2)
  . filter canon
  $ [ (ce, e1, e2)
    | e1 <- es, e2 <- es, e1 /= e2, canon (falseE,e1,e2)
    , typ e1 == typ e2, typ e1 /= boolTy
    , ce <- explain e1 e2
    ]
  where
  (es,pres) = (map C.rep cles, map C.rep clpres)
  explain e1 e2 = D.narrow (\ep -> cequal ep e1 e2) cdg
  cdg = D.fromEdges
      . pairsThat (==>)
      $ filter (\e -> typ e == boolTy && not (isAssignment e)) pres

-- | Is the equation a consequence of substitution?
-- > subConsequence (x == y) (x + y) (x + x) == True
-- > subConsequence (x <= y) (x + y) (x + x) == False -- not sub
-- > subConsequence (abs x == abs y) (abs x) (abs y) == True
-- > subConsequence (abs x == 1) (x + abs x) (20) == False (artificial)
subConsequence :: Thy -> [Class Expr] -> Expr -> Expr -> Expr -> Bool
subConsequence thy clpres ((Constant "==" _ :$ ea) :$ eb) e1 e2
  -- NOTE: the first 4 are uneeded, but make it a bit faster...
  | ea `isSub` e1 && equivalent thy{closureLimit=1} (sub ea eb e1) e2 = True
  | eb `isSub` e1 && equivalent thy{closureLimit=1} (sub eb ea e1) e2 = True
  | ea `isSub` e2 && equivalent thy{closureLimit=1} (sub ea eb e2) e1 = True
  | eb `isSub` e2 && equivalent thy{closureLimit=1} (sub eb ea e2) e1 = True
  | equivalent ((ea,eb) `insert` thy){closureLimit=1} e1 e2 = True
subConsequence thy clpres ce e1 e2 = or
  [ subConsequence thy clpres ce' e1 e2
  | (rce,ces) <- clpres, ce == rce, ce' <- ces ]

psortBy :: (a -> a -> Bool) -> [a] -> [(a,a)]
psortBy (<) xs = [(x,y) | x <- xs, y <- xs, x < y, none (\z -> x < z && z < y) xs]
  where
  none = (not .) . any
