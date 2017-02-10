module Test.Speculate
  ( report

  , Args (..)
  , args
  , speculate
  , getArgs

  , module Test.Speculate.Expr
  , module Test.Speculate.Engine
  , module Test.LeanCheck.Utils
  , module Test.LeanCheck

  , module Data.Typeable
  )
where

import Data.Typeable
import Data.Dynamic
import Data.Maybe
import Data.List hiding (insert)
import qualified Data.List as L
import Data.Function
import Control.Monad
import Test.LeanCheck
import Test.LeanCheck.Utils hiding (comparison)
import Test.LeanCheck.Tiers (unorderedPairsWith)
import Test.Speculate.Utils
import Test.Speculate.Utils.Colour
import Data.Ratio ((%))

import System.Console.CmdArgs.Explicit

import Test.Speculate.Expr
import Test.Speculate.Reason
import Test.Speculate.CondReason
import Test.Speculate.SemiReason
import Test.Speculate.Engine

data Args = Args
  { maxSize              :: Int
  , maxTests             :: Int
  , minTests             :: Int -> Int
  , maxSemiSize          :: Int
  , maxCondSize          :: Int
  , maxDepth             :: Maybe Int
  , instances            :: [Instances]
  , showConstants        :: Bool
  , showTheory           :: Bool
  , showEquations        :: Bool
  , showSemiequations    :: Bool
  , showConditions       :: Bool
  , showConstantLaws     :: Bool
  , showDot              :: Bool
  , quietDot             :: Bool
  , showClassesFor       :: [Int]
  , maxVars              :: Int
  , maxConstants         :: Maybe Int
  , evalTimeout          :: Maybe Double
--, closureLimit         :: Int
--, order                :: OptOrder  -- data OptOrder = Dershowitz | KnuthBendix
--, maxRuleSize          :: Maybe Int
--, maxEquationSize      :: Maybe Int
--, keepRewriteRules     :: Bool
  , showHelp             :: Bool
  , extra                :: [String] -- unused, user-defined meaning
  , backgroundConstants  :: [Expr] -- ^ background constants
  , constants            :: [Expr] -- ^ constants used on both conditions and equations
  , conditionConstants   :: [Expr] -- ^ constants exclusive to conditions
  , equationConstants    :: [Expr] -- ^ constants exclusive to equations
  , exclude              :: [String] -- ^ exclude this symbols from signature before running
  }
-- Maybe add an empty Thy here.

args = Args
  { maxSize              = 5
  , maxTests             = 500
  , minTests             = \n -> n `div` 20 -- defaults to 5% of maxTests
  , maxSemiSize          = -2
  , maxCondSize          = -1
  , maxDepth             = Nothing
  , instances            = []
  , showConstants        = True
  , showTheory           = False
  , showEquations        = True
  , showSemiequations    = True
  , showConditions       = True
  , showConstantLaws     = False
  , showDot              = False
  , quietDot             = False
  , showClassesFor       = []
  , maxVars              = 2
  , maxConstants         = Nothing
  , evalTimeout          = Nothing
--, closureLimit         = 2
--, order                = Dershowitz
--, maxRuleSize          = Nothing
--, maxEquationSize      = Nothing
--, keepRewriteRules     = False
  , showHelp             = False
  , extra                = []
  , backgroundConstants  = []
  , constants            = []
  , conditionConstants   = []
  , equationConstants    = []
  , exclude              = []
  }

computeMaxSemiSize :: Args -> Int
computeMaxSemiSize args
  | maxSemiSize args > 0 = maxSemiSize args
  | otherwise            = maxSize args + maxSemiSize args

computeMaxCondSize :: Args -> Int
computeMaxCondSize args
  | maxCondSize args > 0 = maxCondSize args
  | otherwise            = maxSize args + maxCondSize args

computeInstances :: Args -> Instances
computeInstances args = concat (instances args) ++ preludeInstances

shouldShow2 :: Args -> (Expr,Expr) -> Bool
shouldShow2 args (e1,e2) = showConstantLaws args || hasVar e1 || hasVar e2
-- `allAbout` constants // (conditionAtoms `union` equationAtoms)

shouldShowEquation :: Args -> (Expr,Expr) -> Bool
shouldShowEquation args (e1,e2) =
  shouldShow2 args (e1,e2) && (e1 `about` ea || e2 `about` ea)
  where
  ea = equationConstants args
    ++ ((constants args \\ conditionConstants args) \\ backgroundConstants args)

shouldShow3 :: Args -> (Expr,Expr,Expr) -> Bool
shouldShow3 args (e1,e2,e3) = showConstantLaws args
                           || hasVar e1 || hasVar e2 || hasVar e3

shouldShowConditionalEquation :: Args -> (Expr,Expr,Expr) -> Bool
shouldShowConditionalEquation args (ce,e1,e2) = shouldShow3 args (ce,e1,e2)
                                             && cem ce e1 e2
                                             && (ce `about` ca
                                              || e1 `about` ea
                                              || e2 `about` ea)
  where
  cem = condEqualM (computeInstances args) (maxTests args) (minTests args (maxTests args))
  ca = conditionConstants args ++ ((constants args \\ equationConstants args)  \\ backgroundConstants args)
  ea = equationConstants args  ++ ((constants args \\ conditionConstants args) \\ backgroundConstants args)

keepExpr :: Args -> Expr -> Bool
keepExpr Args{maxConstants = Just n} e | length (consts e) > n = False
keepExpr Args{maxDepth     = Just n} e |         depthE e  > n = False
keepExpr _                           _                         = True

-- | Are all constants in an expression about a list of constants?
-- Examples in pseudo-Haskell:
--
-- > x + y `allAbout` [(+)] == True
-- > x + y == z `allAbout` [(+)] == False
-- > x + y == z `allAbout` [(+),(==)] == True
allAbout :: Expr -> [Expr] -> Bool
e `allAbout` es = atomicConstants e `areAll` (`elem` es)

about :: Expr -> [Expr] -> Bool
e `about` es = atomicConstants e `areAny` (`elem` es)

notAbout :: Expr -> [Expr] -> Bool
notAbout = not .: about

timeout :: Args -> Bool -> Bool
timeout Args{evalTimeout = Nothing} = id
timeout Args{evalTimeout = Just t}  = timeoutToFalse t

report :: Args -> IO ()
report args@Args {maxSize = sz, maxTests = n} = do
  let ti = computeInstances args
  let ds = discard (\c -> any (c `isConstantNamed`) (exclude args))
         $ constants args `union` backgroundConstants args `union` conditionConstants args `union` equationConstants args
  let (ts,uts) = partition (isListable ti) $ nubMergeMap (typesIn . typ) ds
  let showConditions' = showConditions args && boolTy `elem` map (finalResultTy . typ) ds
  let ds' = map holeOfTy ts `union` ds
            `union` [showConstant True  | showConditions' || showDot args]
            `union` [showConstant False | showConditions' || showDot args]
            `union` catMaybes [eqE ti t | t <- ts, showConditions']
  let (thy,es) = theoryAndRepresentativesFromAtoms sz (keepExpr args) (timeout args .: equal ti n) ds'
  when (showConstants args)    . putStrLn . unlines $ map show ds'
  unless (null uts) . putStrLn
    $ unlines ["Warning: no Listable instance for " ++ show t
            ++ ", variables of this type will not be considered"
              | t <- uts]
  when (showTheory args)       . putStrLn $ showThy thy
  when (showEquations args) . putStrLn $ prettyThy (shouldShowEquation args) ti thy
  reportClassesFor ti n (showClassesFor args) thy es
  when (showSemiequations args) . putStrLn
    . prettyShy (shouldShowEquation args) (equivalentInstance thy)
    . semiTheoryFromThyAndReps ti n (maxVars args) thy
    $ filter (\e -> lengthE e <= computeMaxSemiSize args) es
  when showConditions' . putStrLn
    . prettyChy (shouldShowConditionalEquation args)
    $ conditionalTheoryFromThyAndReps ti n (maxVars args) (computeMaxCondSize args) thy es
  when (showDot args) $
    reportDot ti (quietDot args) (maxVars args) n thy es

reportClassesFor :: Instances -> Int -> [Int] -> Thy -> [Expr] -> IO ()
reportClassesFor ti nTests nVarss thy res = do
  mapM_ (putStrLn . unlines . map show . r) nVarss
  mapM_ pn nVarss
  where
  pn 0 = putStrLn $ "Number of Eq schema classes: " ++ show (length $ r 0)
  pn n = putStrLn $ "Number of Eq " ++ show n ++ "-var classes: " ++ show (length $ r n)
  r 0 = filter (isEq ti) res
  r n = distinctFromSchemas ti nTests n thy (r 0)

-- for cmdArgs
prepareArgs :: Args -> Mode Args
prepareArgs args =
  mode "speculate" args "" (flagArg (\s a -> Right a {extra = s:extra a}) "")
  [ "ssize"              --= \s a -> a {maxSize  = read s}
  , "ttests"             --= \s a -> a {maxTests = read s}
  , "mmin-tests"         --= \s a -> a {minTests = parseMinTests s}
  , "zsemisize"          --= \s a -> a {maxSemiSize = read s}
  , "xcondsize"          --= \s a -> a {maxCondSize = read s}
  , "Aconstants"         --.   \a -> a {showConstants = False}
  , "Ttheory"            --.   \a -> a {showTheory = True}
  , "Eno-equations"      --.   \a -> a {showEquations = False}
  , "Sno-semiequations"  --.   \a -> a {showSemiequations = False}
  , "Cno-sideconditions" --.   \a -> a {showConditions = False}
  , "0no-constant-laws"  --.   \a -> a {showConstantLaws = True}
  , "rclass-reps-for"    --= \s a -> a {showClassesFor = read s `L.insert` showClassesFor a}
  , "vvars"              --= \s a -> a {maxVars = read s}
  , "cmax-constants"     --= \s a -> a {maxConstants = Just $ read s}
  , "eeval-timeout"      --= \s a -> a {evalTimeout = Just $ read s}
  , "ddepth"             --= \s a -> a {maxDepth = Just $ read s}
  , "gsemi-digraph"      --.   \a -> a {showDot = True
                                       ,quietDot = False
                                       ,showConstants = False
                                       ,showEquations = False
                                       ,showSemiequations = False
                                       ,showConditions = False}
  , "Dquiet-dot"         --.   \a -> a {showDot = True
                                       ,quietDot = True
                                       ,showConstants = False
                                       ,showEquations = False
                                       ,showSemiequations = False
                                       ,showConditions = False}
  , "hhelp"              --.   \a -> a {showHelp = True}
  , " exclude"           --= \s a -> a {exclude = exclude a ++ splitAtCommas s}
  ]
  where
  (short:long) --= fun = flagReq  [[short],long] ((Right .) . fun) "X" ""
  (short:long) --. fun = flagNone [[short],long] fun                   ""
  parseMinTests :: String -> Int -> Int
  parseMinTests s | last s == '%' = \x -> read (init s) * x `div` 100
                  | otherwise     = const (read s)

speculate :: Args -> IO ()
speculate args = do
  as <- processArgs (prepareArgs args)
  if showHelp as
    then print $ helpText [] HelpFormatDefault (prepareArgs args)
    else report as

getArgs :: Args -> IO Args
getArgs = processArgs . prepareArgs

reportDot :: Instances -> Bool -> Int -> Int -> Thy -> [Expr] -> IO ()
reportDot ti quiet nVars n thy es = do
  let ces = distinctFromSchemas ti n nVars thy
          $ filter (isEqOrd ti) es
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
