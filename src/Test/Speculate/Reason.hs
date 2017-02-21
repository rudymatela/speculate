module Test.Speculate.Reason
  ( Thy (..)
  , emptyThy
  , normalize
  , normalizeE
  , isNormal
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
  , prettyThy
  , criticalPairs
  , normalizedCriticalPairs
  , append
  , difference

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

  , reductions1
  , reductionsO

  , dwoBy
  , (|>)
  )
where

import Test.Speculate.Expr
import Test.Speculate.Reason.Order
import Test.Speculate.Utils
import Data.Either
import Data.Tuple (swap)
import Data.List (partition, (\\), sortBy, sort)
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Functor ((<$>)) -- for GHC < 7.10
import qualified Data.List as L (insert)
import Data.Maybe (fromJust,isJust,listToMaybe,maybeToList,mapMaybe)
import Control.Monad

type Rule = (Expr,Expr)
type Equation = (Expr,Expr)

data Thy = Thy
         { rules :: [Rule]
         , equations :: [Equation]
         , canReduceTo :: Expr -> Expr -> Bool
         , closureLimit :: Int
         , keepE :: Expr -> Bool
         }

-- data invariant
okThy :: Thy -> Bool
okThy thy@Thy {rules = rs, equations = eqs, canReduceTo = (->-), keepE = keep} =
     strictlyOrdered rs
  && strictlyOrdered eqs
  && all (uncurry (->-)) rs
  && all ((/= LT) . uncurry compareComplexity) eqs
  && all (uncurry ((==) `on` typ)) (rs++eqs)
  && all canonicalEqn eqs
  && all canonicalRule rs
-- && canonicalizeThy thy == thy -- (uneeded, follows from above)
  && all keepEqn (rs++eqs)
  where
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
         , closureLimit = 0
         , keepE = const True
         }

ruleFilter :: Thy -> [Rule] -> [Rule]
ruleFilter Thy {keepE = keep} = filter keepR
  where
  keepR (e1,e2) = keep e1 && keep e2

keepUpToLength :: Int -> Expr -> Bool
keepUpToLength limit e = lengthE e <= limit

keepMaxOf :: [Equation] -> Expr -> Bool
keepMaxOf = keepUpToLength . (+1) . maximum . (0:) . map lengthE . catPairs

normalize :: Thy -> Expr -> Expr
normalize Thy {rules = rs} = n
  where
  n e = case concatMap (e `reductions1`) rs of
          []     -> e -- already normalized
          (e':_) -> n e'

-- normalize by rules and equations
normalizeE :: Thy -> Expr -> Expr
normalizeE Thy {rules = rs, equations = eqs, canReduceTo = (->-) } = n
  where
  n e = case concatMap (e `reductions1`) rs
          ++ filter (e ->-) (concatMap (e `reductions1`) $ eqs ++ map swap eqs) of
          []     -> e -- already normalized
          (e':_) -> n e'

isNormal :: Thy -> Expr -> Bool
isNormal thy e = normalizeE thy e == e

reduceRoot :: Expr -> Rule -> Maybe Expr
reduceRoot e (e1,e2) = (e2 `assigning`) <$> (e `match` e1)

-- Lists all reductions by one rule, note that reductions may be repeated.
-- For unrepeated reductions see reductionsO
reductions1 :: Expr -> Rule -> [Expr]
reductions1 e (l,_) | lengthE l > lengthE e = [] -- optional optimization
reductions1 e@(e1 :$ e2) r = maybeToList (e `reduceRoot` r)
                          ++ map (:$ e2) (reductions1 e1 r)
                          ++ map (e1 :$) (reductions1 e2 r)
reductions1 e r = maybeToList (e `reduceRoot` r)

-- Lists all reductions by one rule without repetitions.
-- For a faster version that allows repetitions, see reductions1
reductionsO :: Expr -> Rule -> [Expr]
reductionsO e (l,_) | lengthE l > lengthE e = [] -- optional optimization
reductionsO e@(e1 :$ e2) r = maybeToList (e `reduceRoot` r)
                         +++ map (:$ e2) (reductionsO e1 r)
                         +++ map (e1 :$) (reductionsO e2 r)
reductionsO e r = maybeToList (e `reduceRoot` r)

-- as defined by Martin & Nipkow in "Ordered Rewriting and Confluence" on 1990
-- this definition is sound, but incomplete (some groundJoinable pairs won't be
-- detected).
groundJoinable :: Thy -> Expr -> Expr -> Bool
groundJoinable thy@Thy{equations = eqs} e1 e2 =
     e1 == e2
  || any (\(el,er) -> maybe2 False ((==) `on` sort) (e1 `match` el) (e2 `match` er)) (eqs ++ map swap eqs)
  || (f == g && and (zipWith (groundJoinable thy) xs ys))
  where
  (f:xs) = unfoldApp e1
  (g:ys) = unfoldApp e2
-- TODO: one case missing on groundJoinable
-- I need a function f, such that:
-- f (x) = [x]
-- f (xy) = [xx, xy, yx]
-- f (xyz) = [xxx, xxy, xyx, xyy, xyz, xzy, yxx, yxy, yyx, yzx, zxy, zyx]
-- f (xyzw) = [ xxxx, xxxy, xxyx, xxyy, xxyz, xxzy
--            , xyxx, xyxy, xyyx, xyyy, xyyz, xyzx, xyzy, xyzz, xyzw, xywz
--            , xzxy, xzyx, xzyy, xzyz, xzyw, xzzy, xzwx, xzwy,
--            , xwyz, ... ]

normalizedCriticalPairs :: Thy -> [(Expr,Expr)]
normalizedCriticalPairs thy = nubSort
                            . map canonicalizeEqn
                            . discard (uncurry $ groundJoinable thy)
                            . filter (uncurry (/=))
                            . map (normalize thy *** normalize thy)
                            $ criticalPairs thy

criticalPairs :: Thy -> [(Expr,Expr)]
criticalPairs Thy {rules = rs} =
  nubMerges [r `criticalPairsWith` s | r <- rs, s <- rs]

criticalPairsWith :: Rule -> Rule -> [(Expr,Expr)]
r1@(e1,_) `criticalPairsWith` r2@(e2,_) =
    nubSort
  . map sortuple
  . filter (uncurry (/=))
  . concatMap (\e -> (e `reductions1` r1) ** (e `reductions1` r2))
  $ overlaps e1 e2
  where
  xs ** ys = [(x,y) | x <- xs, y <- ys]
  sortuple (x,y) | x < y     = (y,x)
                 | otherwise = (x,y)

-- Warning: will have to also be applied in reverse to get all overlaps.
--
-- canonicalization here is needed for the nub
overlaps :: Expr -> Expr -> [Expr]
overlaps e1 e2 = nubSort
               . map (canonicalize . (e2' `assigning`))
               $ (e1' `unification`) `mapMaybe` subexprs e2'
  where
  e1' = renameBy (++ "1") e1
  e2' = renameBy (++ "2") e2

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
  reductionsEqs1 e = e `L.insert` (nubMergeMap (reductions1 e) (eqs ++ map swap eqs))

insert :: Equation -> Thy -> Thy
insert (e1,e2) thy
  | normalize thy e1 == normalize thy e2 = thy
  | otherwise = complete $ updateEquationsBy (canonicalizeEqn (e1,e2) `L.insert`) thy

append :: Thy -> [Equation] -> Thy
append thy eqs = updateEquationsBy (nubSort . (++ eqs')) thy
  where
  eqs' = map canonicalizeEqn $ filter (uncurry ((/=) `on` normalize thy)) eqs

difference :: Thy -> Thy -> Thy
difference thy1@Thy {equations = eqs1, rules = rs1}
           thy2@Thy {equations = eqs2, rules = rs2} =
  thy1 {equations = eqs1 \\ eqs2, rules = rs1 \\ rs2}

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
simplify thy = updateEquationsBy (nubSort . map canonicalizeEqn)
             $ mapEquations (normalize thy *** normalize thy) thy

-- a.k.a. R-Simplify-rule
compose :: Thy -> Thy
compose thy = updateRulesBy (nubSort . map canonicalizeRule)
            $ mapRules (id *** normalize thy) thy

-- a.k.a. L-Simplify-rule
collapse :: Thy -> Thy
collapse thy@Thy {equations = eqs, rules = rs} =
  thy {equations = eqs +++ foldr (+++) [] (map collapse eqs'), rules = rs'}
  where
  (eqs',rs') = partition collapsable rs
  collapsable = not . null . collapse
  collapse :: Rule -> [Equation]
  collapse (e1,e2) = foldr (+++) []
                   $ [ nubSort
                     $ canonicalizeEqn
                    <$> (\e -> (e,e2)) <$> reductions1 e1 (e1',e2')
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
canonicalizeThyWith ti = mapRules (canonicalizeRuleWith ti)
                       . mapEquations (canonicalizeEqnWith ti)

canonicalizeEqn :: Equation -> Equation
canonicalizeEqn = canonicalizeEqnWith preludeInstances

canonicalEqn :: Equation -> Bool
canonicalEqn eq = canonicalizeEqn eq == eq

canonicalizeEqnWith :: Instances -> Equation -> Equation
canonicalizeEqnWith ti = swap . canonicalizeRuleWith ti . swap . o
  where
  o (e1,e2) | e1 `compareComplexity` e2 == LT = (e2,e1)
            | otherwise                       = (e1,e2)


canonicalizeRule :: Rule -> Rule
canonicalizeRule = canonicalizeRuleWith preludeInstances

canonicalRule :: Rule -> Bool
canonicalRule r = canonicalizeRule r == r

canonicalizeRuleWith :: Instances -> Rule -> Rule
canonicalizeRuleWith ti (e1,e2) =
  case canonicalizeWith ti (e1 :$ e2) of
    e1' :$ e2' -> (e1',e2')
    _ -> error $ "canonicalizeRuleWith: the impossible happened,"
              ++ "this is definitely a bug, see source!"

printThy :: Thy -> IO ()
printThy = putStrLn . showThy

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

prettyThy :: (Equation -> Bool) -> Instances -> Thy -> String
prettyThy shouldShow ti thy =
    table "r l l" . map showEquation
  . sortOn (typ . fst) . sortOn (lengthE . fst)
  . filter shouldShow
  $ rules thy' ++ (map swap $ equations thy')
  where
  thy' = canonicalizeThyWith ti . discardRedundantRulesByEquations $ finalize thy
  showEquation (e1,e2)
--  | typ e1 == boolTy = [showOpExpr "<==>" e1, "<==>", showOpExpr "<==>" e2]
    | otherwise        = [showOpExpr "==" e1, "==", showOpExpr "==" e2]

-- | Finalize a theory by discarding redundant equations.  If after finalizing
--   you 'complete', redundant equations might pop-up again.
finalize :: Thy -> Thy
finalize = discardRedundantEquations

theorize :: [Equation] -> Thy
theorize = theorizeBy (canReduceTo emptyThy)

theorizeBy :: (Expr -> Expr -> Bool) -> [Equation] -> Thy
theorizeBy (>) = finalize
               . canonicalizeThy
               . complete
               . initialize 3 (>)

initialize :: Int -> (Expr -> Expr -> Bool) -> [Equation] -> Thy
initialize n (>) eqs = emptyThy
                     { equations = nubSort $ map canonicalizeEqn eqs
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
                   . sortOn (uncurry (+) . (lengthE *** lengthE))
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
