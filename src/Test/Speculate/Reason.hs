-- |
-- Module      : Test.Speculate.Reason
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Equational reasoning for 'Expr's based on term rewriting.
module Test.Speculate.Reason
  ( Thy (..)
  , emptyThy
  , normalize
  , normalizeE
  , isNormal
  , isRootNormal
  , isRootNormalE
  , complete
  , equivalent
  , equivalentInstance
  , insert
  , showThy
  , printThy
  , keepUpToLength
  , keepMaxOf
  , (|==|)
  , theorize
  , theorizeBy
  , finalEquations
  , criticalPairs
  , normalizedCriticalPairs
  , append

  , okThy
  , canonicalEqn
  , canonicalRule
  , canonicalizeEqn
  , deduce
  , simplify
  , delete
  , orient
  , compose
  , collapse
  , updateRulesBy
  , updateEquationsBy
  , discardRedundantEquations
  , finalize
  , initialize
  , defaultKeep
  , doubleCheck
  , commutativeOperators

  , reductions1
  , groundJoinable

  , dwoBy
  , (|>)
  )
where

import Test.Speculate.Expr
import Test.Speculate.Reason.Order
import Test.Speculate.Utils
import Data.Either
import Data.Function (on)
import Data.Functor ((<$>)) -- for GHC < 7.10
import Data.List (partition, (\\), sortBy, sort)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Tuple (swap)
import qualified Data.List as L (insert)
import Control.Monad

type Rule = (Expr,Expr)
type Equation = (Expr,Expr)

data Thy = Thy
  { rules :: [Rule]
  , equations :: [Equation]
  , canReduceTo :: Expr -> Expr -> Bool -- ^ should be compatible with compareE
  , compareE :: Expr -> Expr -> Ordering -- ^ total order used to "sort" equations
  , closureLimit :: Int
  , keepE :: Expr -> Bool
  , invalid :: [Equation] -- ^ reserved for rules and equations that were later found to be invalid through testing
  }

compareEqn :: Thy -> Equation -> Equation -> Ordering
compareEqn thy@Thy {compareE = cmp} (e1l,e1r) (e2l,e2r) =
  e1l `cmp` e2l  <>  e1r `cmp` e2r

-- data invariant
okThy :: Thy -> Bool
okThy thy@Thy {rules = rs, equations = eqs, canReduceTo = (->-), keepE = keep, compareE = cmp} =
     orderedBy (<) rs
  && orderedBy (<) eqs
  && all (uncurry (->-)) rs
  && all ((/= LT) . uncurry cmp) eqs
  && all (uncurry ((==) `on` typ)) (rs++eqs)
  && all (canonicalEqn thy) eqs
  && all canonicalRule rs
-- && canonicalizeThy thy == thy -- (uneeded, follows from above)
  && all keepEqn (rs++eqs)
  where
  e1 < e2 = compareEqn thy e1 e2 == LT
  keepEqn (e1,e2) = keep e1 && keep e2

updateRulesBy :: ([Rule] -> [Rule]) -> Thy -> Thy
updateRulesBy f thy@Thy {rules = rs} = thy {rules = f rs}

updateEquationsBy :: ([Equation] -> [Equation]) -> Thy -> Thy
updateEquationsBy f thy@Thy {equations = es} = thy {equations = f es}

mapRules :: (Rule -> Rule) -> Thy -> Thy
mapRules = updateRulesBy . map

mapEquations :: (Equation -> Equation) -> Thy -> Thy
mapEquations = updateEquationsBy . map

-- | This instance is as efficient as it gets, but, this function will not
--   detect equality when rules and equations are in a different order (or
--   repeated).  See '|==|'.
instance Eq Thy where
  t == u = rules t == rules u
        && equations t == equations u
        && closureLimit t == closureLimit u -- useful when self-speculating

(|==|) :: Thy -> Thy -> Bool
(|==|) t u = rules t =|= rules u
          && map orient (equations t) =|= map orient (equations u)
  where
  xs =|= ys = nubSort xs == nubSort ys
  orient (e1,e2) | e1 < e2   = (e2,e1)
                 | otherwise = (e1,e2)
infix 4 |==|

emptyThy :: Thy
emptyThy = Thy
         { rules = []
         , equations = []
         , canReduceTo = (|>)
         , compareE = compare
         , closureLimit = 0
         , keepE = const True
         , invalid = []
         }

ruleFilter :: Thy -> [Rule] -> [Rule]
ruleFilter Thy {keepE = keep} = filter keepR
  where
  keepR (e1,e2) = keep e1 && keep e2

keepUpToLength :: Int -> Expr -> Bool
keepUpToLength limit e = size e <= limit

keepMaxOf :: [Equation] -> Expr -> Bool
keepMaxOf = keepUpToLength . (+1) . maximum . (0:) . map size . catPairs

normalize :: Thy -> Expr -> Expr
normalize Thy {rules = rs} = n
  where
  n e = case concatMap (e `reductions1`) rs of
          []     -> e -- already normalized
          (e':_) -> n e'

-- normalize by rules and equations
normalizeE :: Thy -> Expr -> Expr
normalizeE thy@(Thy {equations = eqs, canReduceTo = (->-)})  =  n1
  where
  n1  =  n2 . normalize thy
  n2 e = case filter (e ->-) (concatMap (e `reductions1`) $ eqs ++ map swap eqs) of
         []     -> e -- already normalized
         (e':_) -> n1 e'

isNormal :: Thy -> Expr -> Bool
isNormal thy e = normalizeE thy e == e

isRootNormal :: Thy -> Expr -> Bool
isRootNormal thy e  =  none (e `isInstanceOf`) $ map fst (rules thy)

isRootNormalE :: Thy -> Expr -> Bool
isRootNormalE thy e  =  isRootNormal thy e
                    &&  null (filter (e ->-) . mapMaybe (reduceRoot e) $ equations thy ++ map swap (equations thy))
  where
  (->-)  =  canReduceTo thy
  reduceRoot e (e1,e2) = (e2 //-) <$> (e `match` e1)

reduceRoot :: Expr -> Rule -> Maybe Expr
reduceRoot e (e1,e2) = (e2 //-) <$> (e `match` e1)

-- Lists all reductions by one rule, note that reductions may be repeated.
reductions1 :: Expr -> Rule -> [Expr]
reductions1 e (l,_) | size l > size e = [] -- optional optimization
reductions1 e@(e1 :$ e2) r = maybeToList (e `reduceRoot` r)
                          ++ map (:$ e2) (reductions1 e1 r)
                          ++ map (e1 :$) (reductions1 e2 r)
reductions1 e r = maybeToList (e `reduceRoot` r)

-- as defined by Martin & Nipkow in "Ordered Rewriting and Confluence" on 1990
-- this definition is sound, but incomplete (some groundJoinable pairs won't be
-- detected).
groundJoinable :: Thy -> Expr -> Expr -> Bool
groundJoinable thy@Thy{rules = rs, equations = eqs} e1 e2 =
     e1 == e2
  || any (\(el,er) -> maybe2 False ((==) `on` sort) (e1 `match` el) (e2 `match` er)) (rs ++ map swap rs ++ eqs ++ map swap eqs)
  || (f == g && and (zipWith (groundJoinable thy) xs ys))
  || all ((\(e1,e2) -> normalize thy e1 == normalize thy e2) . unfoldPair) (constifications $ foldPair (e1,e2))
  where
  (f:xs) = unfoldApp e1
  (g:ys) = unfoldApp e2

normalizedCriticalPairs :: Thy -> [(Expr,Expr)]
normalizedCriticalPairs thy = nubSortBy (compareEqn thy)
                            . map (canonicalizeEqn thy)
                            . discard (uncurry $ groundJoinable thy)
                            . filter (uncurry (/=))
                            . map (normalize thy *** normalize thy)
                            $ criticalPairs thy

criticalPairs :: Thy -> [(Expr,Expr)]
criticalPairs thy@Thy{rules = rs}  =
  nubMergesBy compareEqnQuickly [r `criticalPairsWith` s | r <- rs, s <- rs]
  where
  criticalPairsWith :: Rule -> Rule -> [(Expr,Expr)]
  r1@(e1,_) `criticalPairsWith` r2@(e2,_) =
      nubSortBy compareEqnQuickly
    . map sortuple
    . filter (uncurry (/=))
    . concatMap (\e -> (e `reductions1` r1) ** (e `reductions1` r2))
    $ overlaps e1 e2
  xs ** ys = [(x,y) | x <- xs, y <- ys]
  sortuple (x,y) | x < y     = (y,x)
                 | otherwise = (x,y)
  compareEqnQuickly = compareQuickly `on` foldPair
  (<) :: Expr -> Expr -> Bool
  e1 < e2 = e1 `compareQuickly` e2 == LT
  -- NOTE: will have to also be applied in reverse to get all overlaps.
  overlaps :: Expr -> Expr -> [Expr]
  overlaps e1 e2 = nubSortBy compareQuickly
                 . map (canonicalize . (e2' //-))
                 $ (e1' `unifn`) `mapMaybe` nonVarSubexprs e2'
    where
    nonVarSubexprs = discard isVar . nubSubexprs
    e1' = renameVarsBy (++ "1") e1
    e2' = renameVarsBy (++ "2") e2
    unifn = unificationC (commutativeOperators thy)

equivalent :: Thy -> Expr -> Expr -> Bool
equivalent thy e1 e2 = e1' == e2'
                    || or [ normalizeE thy e1'' == normalizeE thy e2''
                          | e1'' <- closure thy e1'
                          , e2'' <- closure thy e2'
                          ]
  where
  e1' = normalizeE thy e1
  e2' = normalizeE thy e2

equivalentInstance :: Thy -> Expr -> Expr -> Bool
equivalentInstance thy e1 e2 = e1' == e2'
                            || or [ normalizeE thy e1'' `isInstanceOf` normalizeE thy e2''
                                  | e1'' <- closure thy e1'
                                  , e2'' <- closure thy e2'
                                  ]
  where
  e1' = normalizeE thy e1
  e2' = normalizeE thy e2

closure :: Thy -> Expr -> [Expr]
closure thy e = iterateUntilLimit (closureLimit thy) (==) step [normalizeE thy e]
  where
  eqs = equations thy
  step = nubMergeMap reductionsEqs1
  reductionsEqs1 e = e `L.insert` nubMergeMap (reductions1 e) (eqs ++ map swap eqs)

insert :: Equation -> Thy -> Thy
insert (e1,e2) thy
  | normalize thy e1 == normalize thy e2 = thy
  | otherwise = complete $ updateEquationsBy (canonicalizeEqn thy (e1,e2) `L.insert`) thy

append :: Thy -> [Equation] -> Thy
append thy eqs = updateEquationsBy (nubSort . (++ eqs')) thy
  where
  eqs' = [ canonicalizeEqn thy (e1',e2')
         | (e1,e2) <- eqs
         , let e1' = normalize thy e1
         , let e2' = normalize thy e2
         , e1' /= e2'
         ]

complete :: Thy -> Thy
complete = iterateUntil (==)
         $ deduce
         . collapse
         . compose
         . orient
         . deleteGroundJoinable
         . delete
         . simplify
-- TODO: (?) on complete, each step should also return a boolean indicating
--           whether the rule was applied succesfully.  (low priority)

completeVerbose :: Thy -> IO Thy
completeVerbose thy0 = do
  let {thy1 = canonicalizeThy thy0}; unless (thy1 == thy0) $ pr "canonThy" thy1
  let {thy2 = deduce          thy1}; unless (thy2 == thy1) $ pr "deduce"   thy2
  let {thy3 = simplify        thy2}; unless (thy3 == thy2) $ pr "simplify" thy3
  let {thy4 = delete          thy3}; unless (thy4 == thy3) $ pr "delete"   thy4
  let {thy5 = orient          thy4}; unless (thy5 == thy4) $ pr "orient"   thy5
  let {thy6 = compose         thy5}; unless (thy6 == thy5) $ pr "compose"  thy6
  let {thy7 = collapse        thy6}; unless (thy7 == thy6) $ pr "collapse" thy7
  -- threadDelay $ 100 * 1000 -- 100 milisecond delay
  if thy7 /= thy0  then completeVerbose thy7
                   else return          thy7
  where
  pr n = (putStrLn (":: After " ++ n ++ ":") >>)
       . putStrLn . showThy


deduce :: Thy -> Thy
deduce thy = updateEquationsBy (+++ ruleFilter thy (normalizedCriticalPairs thy)) thy

orient :: Thy -> Thy
orient thy@Thy {equations = eqs, rules = rs, canReduceTo = (>)} =
  thy {equations = eqs', rules = rs +++ nubSort (map canonicalizeRule rs')}
  where
  (eqs',rs') = partitionEithers . map o $ ruleFilter thy eqs
  o (e1,e2) | e1 > e2 = Right (e1,e2)
            | e2 > e1 = Right (e2,e1)
            | otherwise = Left (e1,e2)

delete :: Thy -> Thy
delete = updateEquationsBy $ discard (uncurry (==))

deleteEquivalent :: Thy -> Thy
deleteEquivalent thy =
  updateEquationsBy (discard (\(e1,e2) -> equivalent (updateEquationsBy (filter (/= (e1,e2))) thy{closureLimit=1}) e1 e2)) thy

deleteGroundJoinable :: Thy -> Thy
deleteGroundJoinable thy =
  updateEquationsBy (discard (\(e1,e2) -> groundJoinable (updateEquationsBy (filter (/= (e1,e2))) thy) e1 e2)) thy
-- TODO: make deleteGroundJoinable more efficient (it is *very* inneficient right now)

-- a.k.a. Simplify-identity
simplify :: Thy -> Thy
simplify thy = updateEquationsBy (nubSort . map (canonicalizeEqn thy))
             $ mapEquations (normalize thy *** normalize thy) thy

-- a.k.a. R-Simplify-rule
compose :: Thy -> Thy
compose thy = updateRulesBy (nubSort . map canonicalizeRule)
            $ mapRules (second $ normalize thy) thy

-- a.k.a. L-Simplify-rule
collapse :: Thy -> Thy
collapse thy@Thy {equations = eqs, rules = rs} =
  thy {equations = eqs +++ foldr (+++) [] (map collapse eqs'), rules = rs'}
  where
  (eqs',rs') = partition collapsable rs
  collapsable = not . null . collapse
  collapse :: Rule -> [Equation]
  collapse (e1,e2) = foldr (+++) []
    [ nubSort [ canonicalizeEqn thy (e,e2) | e <- reductions1 e1 (e1',e2') ]
    | (e1',e2') <- rs
    , (e1',e2') /= (e1,e2)
    , e1 =| e1' ]
  -- emcompasses or ">" or specialization ordering or duck beak
  (=|) :: Expr -> Expr -> Bool
  e1 =| e2 = e1 `hasInstanceOf` e2
     && not (e2 `hasInstanceOf` e1)

canonicalizeThy :: Thy -> Thy
canonicalizeThy = canonicalizeThyWith preludeInstances

canonicalizeThyWith :: Instances -> Thy -> Thy
canonicalizeThyWith ti thy = mapRules (canonicalizeRuleWith ti)
                           . mapEquations (canonicalizeEqnWith thy ti)
                           $ thy

canonicalizeEqn :: Thy -> Equation -> Equation
canonicalizeEqn thy = canonicalizeEqnWith thy preludeInstances

canonicalEqn :: Thy -> Equation -> Bool
canonicalEqn thy eq = canonicalizeEqn thy eq == eq

canonicalizeEqnWith :: Thy -> Instances -> Equation -> Equation
canonicalizeEqnWith thy ti = swap . canonicalizeRuleWith ti . swap . o
  where
  cmp = compareE thy
  o (e1,e2) | e1 `cmp` e2 == LT = (e2,e1)
            | otherwise         = (e1,e2)


canonicalizeRule :: Rule -> Rule
canonicalizeRule = canonicalizeRuleWith preludeInstances

canonicalRule :: Rule -> Bool
canonicalRule r = canonicalizeRule r == r

canonicalizeRuleWith :: Instances -> Rule -> Rule
canonicalizeRuleWith ti (e1,e2) =
  case canonicalizeWith (lookupNames ti) (e1 :$ e2) of
    e1' :$ e2' -> (e1',e2')
    _ -> error $ "canonicalizeRuleWith: the impossible happened,"
              ++ "this is definitely a bug, see source!"

-- | Prints a 'Thy' (theory) on the console. (cf. 'showThy')
printThy :: Thy -> IO ()
printThy = putStrLn . showThy

-- | Pretty-prints a theory into a string. (cf. 'printThy')
showThy :: Thy -> String
showThy thy = (if null rs
                 then "no rules.\n"
                 else "rules:\n"     ++ showEquations rs)
           ++ (if null eqs
                 then ""
                 else "equations:\n" ++ showEquations eqs)
  where
  thy' = canonicalizeThy thy
  rs = rules thy'
  eqs = equations thy'
  showEquations = unlines . map showEquation
  showEquation (e1,e2) = showExpr e1 ++ " == " ++ showExpr e2

finalEquations :: (Equation -> Bool) -> Instances -> Thy -> [Equation]
finalEquations shouldShow ti thy =
    sortBy (compareTy `on` (typ . fst))
  . sortBy (compareE thy `on` foldPair)
  . filter shouldShow
  $ rules thy' ++ map swap (equations thy')
  where
  thy' = canonicalizeThyWith ti . discardRedundantRulesByEquations $ finalize thy

-- | Finalize a theory by discarding redundant equations.  If after finalizing
--   you 'complete', redundant equations might pop-up again.
finalize :: Thy -> Thy
finalize = discardRedundantEquations

-- | Double-checks a resulting theory moving untrue rules and equations to the
--   invalid list.
--
-- As a side-effect of using testing to conjecturing equations,
-- we may get smaller equations that are obviously incorrect
-- when we consider a bigger (harder-to-test) equation that is incorrect.
--
-- For example, given an incorrect large equation, it may follow that
-- False=True.
--
-- This function can be used to double-check the generated theory.
-- If any equation or rule is discarded, that means the number of tests
-- should probably be increased.
doubleCheck :: (Expr -> Expr -> Bool) -> Thy -> Thy
doubleCheck (===) thy  =  thy
                       {  rules     = filter correct (rules thy)
                       ,  equations = filter correct (equations thy)
                       ,  invalid   = filter (not . correct) (rules thy ++ equations thy)
                       }
  where
  correct  =  uncurry (===)

theorize :: [Equation] -> Thy
theorize = theorizeBy (canReduceTo emptyThy)

theorizeBy :: (Expr -> Expr -> Bool) -> [Equation] -> Thy
theorizeBy (>) = finalize
               . canonicalizeThy
               . complete
               . initialize 3 (>)

initialize :: Int -> (Expr -> Expr -> Bool) -> [Equation] -> Thy
initialize n (>) eqs = thy
  where
  thy = emptyThy
      { equations = nubSort $ map (canonicalizeEqn thy) eqs
      , keepE = keepMaxOf eqs
      , canReduceTo = (>)
      , closureLimit = n
      }

defaultKeep :: Thy -> Thy
defaultKeep thy@Thy {equations = eqs, rules = rs} =
  thy { keepE = keepMaxOf (eqs++rs) }

discardRedundantEquations :: Thy -> Thy
discardRedundantEquations thy =
  updateEquationsBy discardRedundant thy
  where
  discardRedundant = d []
                   . discardLater eqnInstanceOf
                   . reverse
                   . sortOn (uncurry (+) . (size *** size))
  (e1l,e1r) `eqnInstanceOf` (e0l,e0r) = e1l `hasCanonInstanceOf` e0l
                                     && e1r `hasCanonInstanceOf` e0r
                                     || e1l `hasCanonInstanceOf` e0r
                                     && e1r `hasCanonInstanceOf` e0l
  d ks [] = ks
  d ks ((e1,e2):eqs)
    | equivalent thy {equations = eqs} e1 e2 = d          ks  eqs
    | otherwise                              = d ((e1,e2):ks) eqs

discardRedundantRulesByEquations :: Thy -> Thy
discardRedundantRulesByEquations thy = updateRulesBy (d [] . reverse) thy
  where
  d ks [] = ks
  d ks ((e1,e2):rs)
    | equivalent thy {rules = ks++rs} e1 e2 = d          ks  rs
    | otherwise                             = d ((e1,e2):ks) rs

commutativeOperators :: Thy -> [Expr]
commutativeOperators thy  =  [ ef
                             | (ef :$ ex :$ ey, ef' :$ ey' :$ ex') <- equations thy
                             , isConst ef
                             , isVar ex
                             , isVar ey
                             , ex /= ey
                             , ef == ef'
                             , ex == ex'
                             , ey == ey'
                             ]
