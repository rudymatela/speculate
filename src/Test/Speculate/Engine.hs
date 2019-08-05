-- |
-- Module      : Test.Speculate.Engine
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Main engine to process data.
module Test.Speculate.Engine
  ( expansions
  , expansionsOfType
  , expansionsWith
  , mostGeneral
  , mostSpecific

  , theoryAndRepresentativesFromAtoms
  , representativesFromAtoms
  , theoryFromAtoms
  , equivalencesBetween

  , consider
  , distinctFromSchemas
  , classesFromSchemas
  , classesFromSchemasAndVariables

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

import Test.LeanCheck ((\/))
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

canonicalVariationsEqn :: (Expr,Expr) -> [(Expr,Expr)]
canonicalVariationsEqn = filter (uncurry (/=))
                       . map unfoldPair
                       . fastCanonicalVariations
                       . foldPair

-- | List all variable assignments for a given type and list of variables.
expansionsOfType :: Expr -> [String] -> Expr -> [Expr]
expansionsOfType ht vs e = [ fill e [v `varAsTypeOf` ht | v <- vs']
                           | vs' <- placements (countHoles ht e) vs ]
  where
  placements :: Int -> [a] -> [[a]]
  placements 0 xs = [[]]
  placements n xs = [y:ys | y <- xs, ys <- placements (n-1) xs]
  countHoles :: Expr -> Expr -> Int
  countHoles ht e = length [() | h <- holes e, typ h == typ ht]
-- change first argument of expansionsOfType to just [Expr] with the list of
-- possible variables?

expansionsWith :: [Expr] -> Expr -> [Expr]
expansionsWith es = ew (collectOn typ es)
  where
  nam (Value ('_':s) _) = s
  nam _ = "expansionsWith: argument list must only contain vars."
  ew :: [[Expr]] -> Expr -> [Expr]
  ew []        e = [e]
  ew (es:tnss) e = ew tnss
           `concatMap` expansionsOfType (head es) (map nam es) e

-- | List all variable assignments for a given number of variables.
--   It only assign variables to holes (variables with "" as its name).
--
-- > > expansions preludeInstances 2 '(_ + _ + ord _)
-- > [ (x + x) + ord c :: Int
-- > , (x + x) + ord d :: Int
-- > , (x + y) + ord c :: Int
-- > , (x + y) + ord d :: Int
-- > , (y + x) + ord c :: Int
-- > , (y + x) + ord d :: Int
-- > , (y + y) + ord c :: Int
-- > , (y + y) + ord d :: Int ]
expansions :: Instances -> Int -> Expr -> [Expr]
expansions is n e =
  case counts (holes e) of
    []      -> [e]
    (h,c):_ -> expansions is n `concatMap`
               expansionsOfType h (take n (lookupNames is h)) e

-- | List the most general assignment of holes in an expression
mostGeneral :: Expr -> Expr
mostGeneral = head . fastCanonicalVariations -- TODO: make this efficient

-- | List the most specific assignment of holes in an expression
mostSpecific :: Expr -> Expr
mostSpecific = last . fastCanonicalVariations -- TODO: make this efficient

rehole :: Expr -> Expr
rehole (e1 :$ e2)    = rehole e1 :$ rehole e2
rehole e | isVar e   = "" `varAsTypeOf` e
         | otherwise = e

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
theoryFromAtoms :: Int -> (Expr -> Expr -> Ordering) -> (Expr -> Bool) -> (Expr -> Expr -> Bool) -> [[Expr]] -> Thy
theoryFromAtoms sz cmp keep (===) = fst . theoryAndRepresentativesFromAtoms sz cmp keep (===)

representativesFromAtoms :: Int -> (Expr -> Expr -> Ordering) -> (Expr -> Bool) -> (Expr -> Expr -> Bool) -> [[Expr]] -> [[Expr]]
representativesFromAtoms sz cmp keep (===) = snd . theoryAndRepresentativesFromAtoms sz cmp keep (===)

expand :: (Expr -> Bool) -> (Expr -> Expr -> Bool) -> Int -> [Expr] -> (Thy,[[Expr]]) -> (Thy,[[Expr]])
expand keep (===) sz ss (thy,sss) = (complete *** id)
                                  . foldl (flip $ consider (===) sz) (thy,sss)
                                  . concat
                                  . (ss:)
                                  . zipWithReverse (*$*)
                                  $ take sz sss
  where
  fes *$* xes = filter keep $ catMaybes [fe $$ xe | fe <- fes, xe <- xes]

-- | Given atomic expressions, compute theory and representative schema
--   expressions.
theoryAndRepresentativesFromAtoms :: Int
                                  -> (Expr -> Expr -> Ordering)
                                  -> (Expr -> Bool) -> (Expr -> Expr -> Bool)
                                  -> [[Expr]] -> (Thy,[[Expr]])
theoryAndRepresentativesFromAtoms sz cmp keep (===) dss =
  chain [expand keep (===) sz' (dss ! (sz'-1)) | sz' <- reverse [1..sz]] (iniThy,[])
  where
  iniThy = emptyThy { keepE = keepUpToLength sz
                    , closureLimit = 2
                    , canReduceTo = dwoBy (\e1 e2 -> e1 `cmp` e2 == GT)
                    , compareE = cmp
                    }

-- considers a schema
consider :: (Expr -> Expr -> Bool) -> Int -> Expr -> (Thy,[[Expr]]) -> (Thy,[[Expr]])
consider (===) sz s (thy,sss)
  | not (s === s) = (thy,sssWs)  -- uncomparable type
  | rehole (normalizeE thy (mostGeneral s)) `elem` ss = (thy,sss)
  | otherwise =
    ( append thy $ equivalencesBetween (-===-) s s ++ eqs
    , if any (\(e1,e2) -> unrepeatedVars e1 && unrepeatedVars e2) eqs
        then sss
        else sssWs )
    where
    e1 -===- e2  =  normalize thy e1 == normalize thy e2 || e1 === e2
    ss = uptoT sz sss
    sssWs = sss \/ wcons0 sz s
    eqs = concatMap (equivalencesBetween (-===-) s) $ filter (s ===) ss
    wcons0 :: Int -> a -> [[a]]
    wcons0 n s = replicate (n-1) [] ++ [[s]]

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

classesFromSchemasAndVariables :: Thy -> [Expr] -> [Expr] -> [Class Expr]
classesFromSchemasAndVariables thy vs = C.mergesOn (normalizeE thy)
                                      . concatMap (classesFromSchemaAndVariables thy vs)

classesFromSchemaAndVariables :: Thy -> [Expr] -> Expr -> [Class Expr]
classesFromSchemaAndVariables thy vs = C.mergesOn (normalizeE thy)
                                     . map C.fromRep
                                     . filter (null . holes)
                                     . expansionsWith vs

-- Return relevant equivalences between holed expressions:
--
-- > equivalencesBetween basicInstances 500 (_ + _) (_ + _) =
-- >   [i + j == j + i]
equivalencesBetween :: (Expr -> Expr -> Bool) -> Expr -> Expr -> [(Expr,Expr)]
equivalencesBetween (===) e1 e2 = filterRelevant $ canonicalVariationsEqn (e1,e2)
  where
  isInstanceOf'  =  isInstanceOf `on` foldPair
  filterRelevant []      =  []
  filterRelevant (e:es)
    | uncurry (===) e    =  e : filterRelevant (discard (`isInstanceOf'` e) es)
    | otherwise          =  filterRelevant es

semiTheoryFromThyAndReps :: Instances -> Int -> Int
                         -> Thy -> [Expr] -> Shy
semiTheoryFromThyAndReps ti nt nv thy =
    stheorize thy
  . pairsThat (\e1 e2 -> e1 /= e2
                      && typ e1 == typ e2
                      && lessOrEqual ti nt e1 e2)
  . distinctFromSchemas ti nt nv thy
  . filter (isOrd ti)

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
  (cles,clpres) = (id *** filter (\(e,_) -> size e <= csz))
                . partition (\(e,_) -> typ e /= boolTy)
                . filter (isEq ti . fst)
                $ classesFromSchemas ti nt nv thy es'

conditionalEquivalences :: (Expr -> Expr -> Ordering)
                        -> ((Expr,Expr,Expr) -> Bool)
                        -> (Expr -> Expr -> Expr -> Bool)
                        -> (Expr -> Expr -> Bool)
                        -> Int -> Thy -> [Class Expr] -> [Class Expr] -> Chy
conditionalEquivalences cmp canon cequal (==>) csz thy clpres cles =
    cdiscard (\(ce,e1,e2) -> subConsequence thy clpres ce e1 e2)
  . foldl (flip cinsert) (Chy [] cdg clpres thy)
  . sortBy (cmp `on` foldTrio)
  . discard (\(pre,e1,e2) -> pre == val False
                          || length (nubVars pre \\ (nubVars e1 +++ nubVars e2)) > 0
                          || subConsequence thy [] pre e1 e2)
  . filter canon
  $ [ (ce, e1, e2)
    | e1 <- es, e2 <- es, e1 /= e2, canon (val False,e1,e2)
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
subConsequence thy clpres ((Value "==" _ :$ ea) :$ eb) e1 e2
  -- NOTE: the first 4 are uneeded, but make it a bit faster...
  | ea `isSubexprOf` e1 && equivalent thy{closureLimit=1} (sub ea eb e1) e2 = True
  | eb `isSubexprOf` e1 && equivalent thy{closureLimit=1} (sub eb ea e1) e2 = True
  | ea `isSubexprOf` e2 && equivalent thy{closureLimit=1} (sub ea eb e2) e1 = True
  | eb `isSubexprOf` e2 && equivalent thy{closureLimit=1} (sub eb ea e2) e1 = True
  | equivalent ((ea,eb) `insert` thy){closureLimit=1} e1 e2 = True
subConsequence thy clpres ce e1 e2 = or
  [ subConsequence thy clpres ce' e1 e2
  | (rce,ces) <- clpres, ce == rce, ce' <- ces ]

psortBy :: (a -> a -> Bool) -> [a] -> [(a,a)]
psortBy (<) xs = [(x,y) | x <- xs, y <- xs, x < y, none (\z -> x < z && z < y) xs]
  where
  none = (not .) . any
