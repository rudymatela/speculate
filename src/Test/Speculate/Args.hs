{-# Language DeriveDataTypeable #-} -- for GHC <= 7.8
-- |
-- Module      : Test.Speculate.CondReason
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part o Speculate.
--
-- Arguments to the 'speculate' function and parsing of command line arguments.
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

  , foregroundConstants
  , backgroundConstants

  , about

  , allAbout

  -- TODO: remove the following exports eventually:
  , prepareArgs
  , module System.Console.CmdArgs.Explicit
  )
where

import Test.Speculate.Expr
import Test.Speculate.Utils
import System.Console.CmdArgs.Explicit

import Test.LeanCheck ((\/))
import qualified Data.List as L (insert)
import Data.List hiding (insert)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))

import Data.Maybe

-- | Arguments to Speculate
data Args = Args
  { maxSize     :: Int         -- ^ maximum size of considered expressions
  , maxTests    :: Int         -- ^ maximum number of test for each law
  , constants   :: [Expr]      -- ^ constants considered when generating expressions
  , instances   :: [Instances] -- ^ typeclass instance information for @Eq@, @Ord@ and @Listable@
  , maxSemiSize :: Int         -- ^ maximum size of inqualities RHS/LHS
  , maxCondSize :: Int         -- ^ maximum size of considered condition
  , maxVars     :: Int         -- ^ maximum number of variables allowed in inequalities and conditional equations

  , showConstants     :: Bool  -- ^ repeat constants on output
  , showEquations     :: Bool  -- ^ whether to show equations
  , showSemiequations :: Bool  -- ^ whether to show inequalties
  , showConditions    :: Bool  -- ^ whether to show conditional equations
  , showConstantLaws  :: Bool  -- ^ whether to show laws with no variables
  , autoConstants     :: Bool  -- ^ automatically include constants taken from tiers of values

  , minTests    :: Int -> Int  -- ^ __(intermediary)__ minimum number of tests
                               --   for passing postconditions in function of
                               --   maximum number of tests
  , maxConstants :: Maybe Int  -- ^ __(intermediary)__ maximum nubmer of constants allowed when considering expressions
  , maxDepth     :: Maybe Int  -- ^ __(intermediary)__ maximum depth of considered expressions
  , showCounts   :: Bool       -- ^ __(intermediary)__ show counts of equations, inequalities and conditional equations
  , showTheory   :: Bool       -- ^ __(debug)__ whether to show raw theory
  , showArgs     :: Bool       -- ^ __(debug)__ show _this_ args before running
  , showHelp     :: Bool       -- ^ __(advanced)__ whether to show the command line help
  , evalTimeout :: Maybe Double -- ^ __(advanced)__ timeout when evaluating ground expressions
  , force        :: Bool       -- ^ __(advanced)__ ignore errors
  , extra        :: [String]   -- ^ __(advanced)__ unused, user-defined meaning
  , exclude      :: [String]   -- ^ __(advanced)__ exclude this symbols from signature before running
  , onlyTypes    :: [String]   -- ^ __(advanced)__ only allow those types at top-level equations / semi-equations
  , showClassesFor :: [Int]    -- ^ __(advanced)__ show equivalence classes of expressions
  , showDot      :: Bool       -- ^ __(advanced)__ whether to show a Graphviz dotfile with an Ord lattice
  , quietDot     :: Bool       -- ^ __(advanced)__ whether to show a Graphviz dotfiel with an Ord lattice (less verbose)
  }
  deriving Typeable -- for GHC <= 7.8
-- TODO: future options:
--, closureLimit      :: Int
--, order             :: OptOrder  -- data OptOrder = Dershowitz | KnuthBendix
--, maxRuleSize       :: Maybe Int
--, maxEquationSize   :: Maybe Int
--, keepRewriteRules  :: Bool
-- Maybe add an empty Thy here.

-- | Default arguments to Speculate
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
  , autoConstants        = False
  , showArgs             = True
  , showTheory           = False
  , showEquations        = True
  , showSemiequations    = True
  , showConditions       = True
  , showConstantLaws     = False
  , showCounts           = False
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
keepExpr Args{maxConstants = Just n} e | length (nubConsts e) > n = False
keepExpr Args{maxDepth     = Just n} e |             depth e  > n = False
keepExpr _                           _                            = True

reallyShowConditions :: Args -> Bool
reallyShowConditions args = showConditions args
                         && boolTy `elem` map (finalResultTy . typ) (allConstants args)

atoms :: Args -> [[Expr]]
atoms args = [ nubSort (mapMaybe (maybeHoleOfTy is) ts)
       `union` allConstants args
       `union` [val True  | showConds || showDot args]
       `union` [val False | showConds || showDot args]
       `union` (nubSort . catMaybes) [lookupComparison "==" t is | showConds, t <- ts] ]
         \-/ foldr (\/) [] [tiersE is t | autoConstants args, t <- ts]
  where
  ts = types args
  is = computeInstances args
  showConds = reallyShowConditions args
  []  \-/ []   =  []
  xss \-/ []   =  xss
  []  \-/ yss  =  yss
  (xs:xss) \-/ (ys:yss)  =  xs `union` ys  :  xss \-/ yss

-- misnomer: these are not all types, but just the star kinded ones...
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
e `allAbout` es = nubConsts e `areAll` (`elem` es)

about :: Expr -> [Expr] -> Bool
e `about` es = nubConsts e `areAny` (`elem` es)

timeout :: Args -> Bool -> Bool
timeout Args{evalTimeout = Nothing} = id
timeout Args{evalTimeout = Just t}  = timeoutToFalse t

-- needs lexicompareBy
compareExpr :: Args -> Expr -> Expr -> Ordering
compareExpr args = compareComplexity <> lexicompareBy cmp
  where
  e1 `cmp` e2 | arity e1 == 0 && arity e2 /= 0 = LT
  e1 `cmp` e2 | arity e1 /= 0 && arity e2 == 0 = GT
  e1 `cmp` e2 = compareIndex (concat $ atoms args) e1 e2 <> e1 `compare` e2
-- NOTE: "concat $ atoms args" may be an infinite list.  This function assumes
-- that the symbols will appear on the list eventually for termination.  If I
-- am correct this ivariant is assured by the rest of the code.

-- | A special 'Expr' value.
--   When provided on the 'constants' list, 
--   makes all the following constants 'foreground' constants.
foreground :: Expr
foreground = constant "foreground" (undefined :: Args)

-- | A special 'Expr' value.
--   When provided on the 'constants' list,
--   makes all the following constants 'background' constants.
--   Background constants can appear in laws about other constants, but not by
--   themselves.
background :: Expr
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
  , "Uauto-constants"    --.   \a -> a {autoConstants = True}
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
  , " counts"            --.   \a -> a {showCounts = True}
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
  (short:long) --= fun = flagReq  (filter (/= " ") [[short],long]) ((Right .) . fun) "X" ""
  _            --= _   = error "(--=): first argument can't be \"\""
  (short:long) --. fun = flagNone (filter (/= " ") [[short],long]) fun                   ""
  _            --. _   = error "(--.): first argument can't be \"\""
  parseMinTests :: String -> Int -> Int
  parseMinTests s | last s == '%' = \x -> read (init s) * x `div` 100
                  | otherwise     = const (read s)

getArgs :: Args -> IO Args
getArgs = processArgs . prepareArgs
