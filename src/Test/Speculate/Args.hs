module Test.Speculate.Args
  ( Args (..)
  , args

  , foreground
  , background

  , getArgs
  , computeMaxSemiSize
  , computeMaxCondSize
  , computeInstances
  , types
  , atoms
  , compareExpr
  , keepExpr
  , timeout
  , shouldShowEquation
  , shouldShowConditionalEquation
  , reallyShowConditions

  -- TODO: remove the following exports eventually:
  , prepareArgs
  , module System.Console.CmdArgs.Explicit
  )
where

import Test.Speculate.Expr
import Test.Speculate.Utils
import System.Console.CmdArgs.Explicit

import qualified Data.List as L (insert)
import Data.List hiding (insert)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))


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
  , showArgs             :: Bool
  , evalTimeout          :: Maybe Double
--, closureLimit         :: Int
--, order                :: OptOrder  -- data OptOrder = Dershowitz | KnuthBendix
--, maxRuleSize          :: Maybe Int
--, maxEquationSize      :: Maybe Int
--, keepRewriteRules     :: Bool
  , showHelp             :: Bool
  , force                :: Bool  -- ^ ignore errors
  , extra                :: [String] -- unused, user-defined meaning
  , constants            :: [Expr] -- ^ constants used on both conditions and equations
  , exclude              :: [String] -- ^ exclude this symbols from signature before running
  , onlyTypes            :: [String] -- ^ only allow those types at top-level equations / semi-equations
  }
-- Maybe add an empty Thy here.

args :: Args
args = Args
  { maxSize              = 5
  , maxTests             = 500
  , minTests             = \n -> n `div` 20 -- defaults to 5% of maxTests
  , maxSemiSize          = -1
  , maxCondSize          = -1
  , maxDepth             = Nothing
  , instances            = []
  , showConstants        = True
  , showArgs             = True
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
  , force                = False
  , extra                = []
  , constants            = []
  , exclude              = []
  , onlyTypes            = []
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
  shouldShow2 args (e1,e2) && (e1 `about` fore || e2 `about` fore)
  where
  fore = foregroundConstants args

shouldShow3 :: Args -> (Expr,Expr,Expr) -> Bool
shouldShow3 args (e1,e2,e3) = showConstantLaws args
                           || hasVar e1 || hasVar e2 || hasVar e3

shouldShowConditionalEquation :: Args -> (Expr,Expr,Expr) -> Bool
shouldShowConditionalEquation args (ce,e1,e2) = shouldShow3 args (ce,e1,e2)
                                             && cem ce e1 e2
                                             && (ce `about` fore
                                              || e1 `about` fore
                                              || e2 `about` fore)
  where
  cem = condEqualM (computeInstances args) (maxTests args) (minTests args (maxTests args))
  fore = foregroundConstants args

keepExpr :: Args -> Expr -> Bool
keepExpr Args{maxConstants = Just n} e | length (consts e) > n = False
keepExpr Args{maxDepth     = Just n} e |         depthE e  > n = False
keepExpr _                           _                         = True

reallyShowConditions :: Args -> Bool
reallyShowConditions args = showConditions args
                         && boolTy `elem` map (finalResultTy . typ) (allConstants args)

atoms :: Args -> [Expr]
atoms args = map holeOfTy ts
     `union` allConstants args
     `union` [showConstant True  | showConds || showDot args]
     `union` [showConstant False | showConds || showDot args]
     `union` catMaybes [eqE (computeInstances args) t | t <- ts, showConds]
  where
  ts = types args
  showConds = reallyShowConditions args

types :: Args -> [TypeRep]
types = nubMergeMap (typesIn . typ) . allConstants

foregroundConstants, backgroundConstants :: Args -> [Expr]
foregroundConstants = fst . partitionByMarkers foreground background . constants
backgroundConstants = snd . partitionByMarkers foreground background . constants

allConstants :: Args -> [Expr]
allConstants args = discard (\c -> any (c `isConstantNamed`) (exclude args))
                  $ discard (\e -> e == foreground || e == background)
                  $ constants args

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

-- needs lexicompareBy
compareExpr :: Args -> Expr -> Expr -> Ordering
compareExpr args = compareComplexityThen (lexicompareBy cmp)
  where
  e1 `cmp` e2 | arity e1 == 0 && arity e2 /= 0 = LT
  e1 `cmp` e2 | arity e1 /= 0 && arity e2 == 0 = GT
  e1 `cmp` e2 = compareIndex (atoms args) e1 e2 <> e1 `compare` e2

foreground, background :: Expr
foreground = constant "foreground" (undefined :: Args)
background = constant "background" (undefined :: Args)
-- NOTE: Hack!  TODO: add reason why

-- for cmdArgs
prepareArgs :: Args -> Mode Args
prepareArgs args =
  mode "speculate" args "" (flagArg (\s a -> Right a {extra = s:extra a}) "")
  [ "ssize"              --= \s a -> a {maxSize  = read s}
  , "ttests"             --= \s a -> a {maxTests = read s}
  , "mmin-tests"         --= \s a -> a {minTests = parseMinTests s}
  , "zsemisize"          --= \s a -> a {maxSemiSize = read s}
  , "xcondsize"          --= \s a -> a {maxCondSize = read s}
  , "Aconstants"         --.   \a -> a {showConstants = False} -- TODO: fix name
  , "Ohide-args"         --.   \a -> a {showArgs = False}
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
                                       ,showConditions = False
                                       ,showArgs = False}
  , "Dquiet-dot"         --.   \a -> a {showDot = True
                                       ,quietDot = True
                                       ,showConstants = False
                                       ,showEquations = False
                                       ,showSemiequations = False
                                       ,showConditions = False
                                       ,showArgs = False}
  , " only-types"        --= \s a -> a {onlyTypes = onlyTypes a ++ splitAtCommas s}
  , "fforce"             --.   \a -> a {force = True}
  , "hhelp"              --.   \a -> a {showHelp = True}
  , " exclude"           --= \s a -> a {exclude = exclude a ++ splitAtCommas s}
  , "aall-foreground"    --.   \a -> a {constants = discard (== background) (constants a)}
  ]
  where
  (short:long) --= fun = flagReq  [[short],long] ((Right .) . fun) "X" ""
  (short:long) --. fun = flagNone [[short],long] fun                   ""
  parseMinTests :: String -> Int -> Int
  parseMinTests s | last s == '%' = \x -> read (init s) * x `div` 100
                  | otherwise     = const (read s)
-- TODO: implement space char semantics

getArgs :: Args -> IO Args
getArgs = processArgs . prepareArgs

