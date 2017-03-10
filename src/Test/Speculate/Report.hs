module Test.Speculate.Report
  ( report
  )
where

import Test.Speculate.Expr
import Test.Speculate.Reason
import Test.Speculate.SemiReason
import Test.Speculate.CondReason
import Test.Speculate.Engine
import Test.Speculate.Sanity
import Test.Speculate.Args
import Test.Speculate.Utils
import Test.Speculate.Utils.Colour
import Test.Speculate.Pretty

import Data.Ratio ((%))
import Control.Monad (when,unless)
import Test.LeanCheck.Utils ((&&&&))
import Data.List (intercalate)

report :: Args -> IO ()
report args@Args {maxSize = sz, maxTests = n} = do
  let ti = computeInstances args
  let ats = types args
  let ts = filter (isListable ti) ats
  let ds' = atoms args
  let (thy,es) = theoryAndRepresentativesFromAtoms sz (compareExpr args) (keepExpr args) (timeout args .: equal ti n) ds'
  putArgs args
  when (showConstants args)    . putStrLn . unlines $ map show ds'
  warnMissingInstances ti ats
  let ies = instanceErrors ti n ats
  unless (null ies) $ do
    let pref | force args = "Warning: "
             | otherwise  = "Error: "
    putStrLn . unlines . map (pref ++) $ ies
    unless (force args) $ do
      putStrLn "There were instance errors, refusing to run."
      putStrLn "Use `--force` or `args{force=true}` to ignore instance errors."
      fail "exiting"
  when (showTheory args)       . putStrLn $ showThy thy
  when (showEquations args) . putStrLn $ prettyThy (shouldShowEquation args) ti thy
  reportClassesFor ti n (showClassesFor args) thy es
  when (showSemiequations args) . putStrLn
    . prettyShy (shouldShowEquation args) ti (equivalentInstance thy)
    . semiTheoryFromThyAndReps ti n (maxVars args) thy
    $ filter (\e -> lengthE e <= computeMaxSemiSize args) es
  when (reallyShowConditions args) . putStrLn
    . prettyChy (shouldShowConditionalEquation args)
    $ conditionalTheoryFromThyAndReps ti (compareExpr args) n (maxVars args) (computeMaxCondSize args) thy es
  when (showDot args) $
    reportDot ti (onlyTypes args) (quietDot args) (maxVars args) n thy es

putArgs :: Args -> IO ()
putArgs args = when (showArgs args) $ do
  let sz = maxSize args
  let isz = computeMaxSemiSize args
  let csz = computeMaxCondSize args
  putOpt "max expr size" sz
  when (isz /= sz) $ putOpt "  |- on ineqs" isz
  when (csz /= sz) $ putOpt "  |- on conds"  csz
  case maxDepth args of
    Nothing -> return ()
    Just d  -> putOpt "max expr depth" (show d)
  putOpt "max  #-tests" (maxTests args)
  when (showConditions args) $
    putOptSuffix "min  #-tests"  (minTests args $ maxTests args) "  (to consider p ==> q true)"
  putOptSuffix "max  #-vars" (maxVars args) "  (for inequational and conditional laws)"
  case evalTimeout args of
    Nothing -> return ()
    Just t  -> putOptSuffix "eval timeout" t "s"
  putStrLn ""
  where
  putOpt :: Show a => String -> a -> IO ()
  putOpt  s x   = putOptSuffix s x ""
  putOptSuffix :: Show a => String -> a -> String -> IO ()
  putOptSuffix s x p = putStrLn $ alignLeft 14 s ++ " = " ++ alignRight 4 (show x) ++ p

warnMissingInstances :: Instances -> [TypeRep] -> IO ()
warnMissingInstances is ts = putLines
  $  ["Warning: no Listable instance for " ++ show t ++
      ", variables of this type will not be considered"
     | t <- ts, not (isListable is t)]
  ++ ["Warning: no Eq instance for " ++ show t ++
      ", equations of this type will not be considered"
     | t <- ts, not (isEq is t)]
  ++ ["Warning: no Ord instance for " ++ show t ++
      ", inequations of this type will not be considered"
     | t <- ts, not (isOrd is t)]

reportClassesFor :: Instances -> Int -> [Int] -> Thy -> [Expr] -> IO ()
reportClassesFor ti nTests nVarss thy res = do
  mapM_ (putStrLn . unlines . map show . r) nVarss
  mapM_ pn nVarss
  where
  pn 0 = putStrLn $ "Number of Eq schema classes: " ++ show (length $ r 0)
  pn n = putStrLn $ "Number of Eq " ++ show n ++ "-var classes: " ++ show (length $ r n)
  r 0 = filter (isEqE ti) res
  r n = distinctFromSchemas ti nTests n thy (r 0)

reportDot :: Instances -> [String] -> Bool -> Int -> Int -> Thy -> [Expr] -> IO ()
reportDot ti onlyTypes quiet nVars n thy es = do
  let ces = distinctFromSchemas ti n nVars thy
          $ (if null onlyTypes
               then id
               else filter ((`elem` map (map toLower) onlyTypes) . map toLower . show . typ))
          $ filter (isEqOrdE ti) es
  let res = [(trueRatio ti n e, e) | e <- ces, typ e == boolTy]
  putStrLn "digraph G {"
  putStrLn "  rankdir = BT"
  putStrLn . unlines
           . map showExprEdge
           . psortBy ((/=) &&&& lessOrEqual ti n)
           $ ces
  unless quiet . putStrLn . unlines
           . map (\(r,e) -> showExprNode e
                         ++ " [style=filled, fillcolor = \""
                         ++ showNodeColour (length (vars e) % (nVars*2)) r
                         ++ "\"]")
           . filter (\(r,e) -> typ e == boolTy)
           $ res
  putStrLn . unlines
           . map (\e -> "  " ++ showExprNode e ++ " [shape=box]")
           . filter isEquation
           . map snd
           $ res
--let rs = sort $ map fst ress
--putStrLn . unlines $ zipWith (\r1 r2 -> "\"" ++ show r1 ++ "\" -> \"" ++ show r2 ++ "\"") rs (tail rs)
--putStrLn . unlines $ map showRank $ collectSndByFst res
  putStrLn "}"
  where
  showRank (r,es) = "  { rank = same; " ++ "\"" ++ show r ++ "\""
                 ++ intercalate "; " (map showExprNode es)
                 ++ " }"
  showExprEdge (e1,e2) = "  " ++ showExprNode e1 ++ " -> " ++ showExprNode e2
  showExprNode e
    | typ e == boolTy && not quiet = let tre = trueRatio ti n e
                                     in "\"" ++ showExpr e
                                    ++ "\\n" ++ showRatio tre
                                    ++ "\\n" ++ show (percent tre) ++ "%\""
    | otherwise = "\"" ++ showExpr e ++ "\""
  showNodeColour varRatio trueRatio =
    showRGB $ fromHSV (hue0 blue) (frac $ coerceRatio varRatio) 1
        `mix` fromHSV (hue0 orange) (1 - frac (coerceRatio trueRatio)) 1
        `mix` white
