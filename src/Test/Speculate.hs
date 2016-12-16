module Test.Speculate
  ( report

  , Args (..)
  , args
  , speculate
  , getArgs

  , module Test.Speculate.Expr
  , module Test.Speculate.Engine
  , module Test.LeanCheck.Utils

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
import Test.Speculate.Utils.Color
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
  , maxSemiSize          :: Int
  , maxCondSize          :: Int
  , typeInfo_            :: TypeInfo
  , showAtoms            :: Bool
  , showTheory           :: Bool
  , showEquivalences     :: Bool
  , showSemiequivalences :: Bool
  , showConditions       :: Bool
  , showConstantLaws     :: Bool
  , showDot              :: Bool
  , showClassesFor       :: [Int]
  , maxVars              :: Int
--, closureLimit         :: Int
--, order                :: OptOrder  -- data OptOrder = Dershowitz | KnuthBendix
--, maxRuleSize          :: Maybe Int
--, maxEquationSize      :: Maybe Int
--, keepRewriteRules     :: Bool
  , showHelp             :: Bool
  , extra                :: [String] -- unused, user-defined meaning
  , atoms                :: [Expr]
  }
-- Maybe add an empty Thy here.

args = Args
  { maxSize              = 5
  , maxTests             = 500
  , maxSemiSize          = -2
  , maxCondSize          = -1
  , typeInfo_            = basicTypeInfo
  , showAtoms            = True
  , showTheory           = False
  , showEquivalences     = True
  , showSemiequivalences = True
  , showConditions       = False -- flip to True afterwards
  , showDot              = False
  , showConstantLaws     = True
  , showClassesFor       = []
  , maxVars              = 3
--, closureLimit         = 2
--, order                = Dershowitz
--, maxRuleSize          = Nothing
--, maxEquationSize      = Nothing
--, keepRewriteRules     = False
  , showHelp             = False
  , extra                = []
  , atoms                = []
  }

computeMaxSemiSize :: Args -> Int
computeMaxSemiSize args
  | maxSemiSize args > 0 = maxSemiSize args
  | otherwise            = maxSize args + maxSemiSize args

computeMaxCondSize :: Args -> Int
computeMaxCondSize args
  | maxCondSize args > 0 = maxCondSize args
  | otherwise            = maxSize args + maxCondSize args

shouldShow2 :: Args -> (Expr,Expr) -> Bool
shouldShow2 args (e1,e2) = showConstantLaws args || hasVar e1 || hasVar e2

shouldShow3 :: Args -> (Expr,Expr,Expr) -> Bool
shouldShow3 args (e1,e2,e3) = showConstantLaws args
                           || hasVar e1 || hasVar e2 || hasVar e3

-- | Are all atoms in an expression about a list of atoms?
-- Examples in pseudo-Haskell:
--
-- > x + y `allAbout` [(+)] == True
-- > x + y == z `allAbout` [(+)] == False
-- > x + y == z `allAbout` [(+),(==)] == True
allAbout :: Expr -> [Expr] -> Bool
e `allAbout` es = atomicConstants e `areAll` (`elem` es)

report :: Args -> IO ()
report args@Args {maxSize = sz, typeInfo_ = ti, maxTests = n, atoms = ds} = do
  -- TODO: use typs here?
  let (ts,uts) = partition (existsInfo ti) $ nubMergeMap (typesIn . typ) ds
  let ds' = map holeOfTy ts `union` ds
  let (thy,es) = theoryAndRepresentativesFromAtoms sz (equal ti n) ds'
  when (showAtoms args)        . putStrLn . unlines $ map show ds'
  unless (null uts) . putStrLn
    $ unlines ["Warning: no typeInfo about " ++ show t
            ++ ", variables of this type will not be considered"
              | t <- uts]
  when (showTheory args)       . putStrLn $ showThy thy
  when (showEquivalences args) . putStrLn $ prettyThy (shouldShow2 args) ti thy
  reportClassesFor ti n (showClassesFor args) thy es
  when (showSemiequivalences args) . putStrLn
    . prettyShy (shouldShow2 args) (equivalentInstance thy)
    . semiTheoryFromThyAndReps ti n (maxVars args) thy
    $ filter (\e -> lengthE e <= computeMaxSemiSize args) es
  when (showConditions args) . putStrLn
    . prettyChy (shouldShow3 args)
    $ conditionalTheoryFromThyAndReps ti n (maxVars args) (computeMaxCondSize args) thy es
  when (showDot args) $
    reportDot ti (maxVars args) n thy es
-- TODO: always append basicTypeInfo to provided typeInfo

reportClassesFor :: TypeInfo -> Int -> [Int] -> Thy -> [Expr] -> IO ()
reportClassesFor ti nTests nVarss thy res = do
  mapM_ (putStrLn . unlines . map show . r) nVarss
  mapM_ pn nVarss
  where
  pn 0 = putStrLn $ "Number of comparable schema classes: " ++ show (length $ r 0)
  pn n = putStrLn $ "Number of comparable  " ++ show n ++ "-var classes: " ++ show (length $ r n)
  r 0 = filter (isComparable ti) res
  r n = distinctFromSchemas ti nTests n thy (r 0)

-- for cmdArgs
prepareArgs :: Args -> Mode Args
prepareArgs args =
  mode "hello" args "" (flagArg (\s a -> Right a {extra = s:extra a}) "")
  [ "ssize"               --= \s a -> a {maxSize  = read s}
  , "ttests"              --= \s a -> a {maxTests = read s}
  , "zsemisize"           --= \s a -> a {maxSemiSize = read s}
  , "xcondsize"           --= \s a -> a {maxCondSize = read s}
  , "Aatoms"              --.   \a -> a {showAtoms = False}
  , "Ttheory"             --.   \a -> a {showTheory = True}
  , "Enoequivalences"     --.   \a -> a {showEquivalences = False}
  , "Snosemiequivalences" --.   \a -> a {showSemiequivalences = False}
  , "Cnosideconditions"   --.   \a -> a {showConditions = True}
  , "0noconstantlaws"     --.   \a -> a {showConstantLaws = False}
  , "cclasses-for"        --= \s a -> a {showClassesFor = read s `L.insert` showClassesFor a}
  , "vvars"               --= \s a -> a {maxVars = read s}
  , "ddot"                --.   \a -> a {showDot = True
                                        ,showAtoms = False
                                        ,showEquivalences = False
                                        ,showSemiequivalences = False
                                        ,showConditions = False}
  , "hhelp"               --.   \a -> a {showHelp = True}
  ]
  where
  (short:long) --= fun = flagReq  [[short],long] ((Right .) . fun) "X" ""
  (short:long) --. fun = flagNone [[short],long] fun                   ""

speculate :: Args -> IO ()
speculate args = do
  as <- processArgs (prepareArgs args)
  if showHelp as
    then print $ helpText [] HelpFormatDefault (prepareArgs args)
    else report as

getArgs :: Args -> IO Args
getArgs = processArgs . prepareArgs

reportDot :: TypeInfo -> Int -> Int -> Thy -> [Expr] -> IO ()
reportDot ti nVars n thy es = do
  let ces = distinctFromSchemas ti n nVars thy
          $ filter (isComparable ti) es
  let res = [(trueRatio ti n e, e) | e <- ces, typ e == boolTy]
  putStrLn "digraph G {"
  putStrLn "  rankdir = BT"
  putStrLn . unlines
           . map showExprEdge
           . psortBy ((/=) &&&& lessOrEqual ti n)
           $ ces
  putStrLn . unlines
           . map (\(r,e) -> showExprNode e
                         ++ " [style=filled, fillcolor = \""
                         ++ showNodeColor (length (vars e) % (nVars*2)) r
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
    | typ e == boolTy = let tre = trueRatio ti n e
                        in "\"" ++ showExpr e
                       ++ "\\n" ++ showRatio tre
                       ++ "\\n" ++ show (percent tre) ++ "%\""
    | otherwise = "\"" ++ showExpr e ++ "\""
  showNodeColor varRatio trueRatio =
    showRGB $ fromHSV (hue blue) (frac $ coerceRatio varRatio) 1
        `mix` fromHSV (hue orange) (1 - frac (coerceRatio trueRatio)) 1
        `mix` white
