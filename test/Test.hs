-- | This module defines utilities to test 'Speculate' itself.
--
-- It should never be exported in a cabal package, and should not be included
-- in Haddock documentation.  Hence the weird name, simply "Test".
--
-- This module exports a Listable Expr instance, that does not, by any means,
-- list all possible expressions.  But instead, list expressions based on the
-- names exported by this module.
module Test
  (
  -- * Module exports
    module Test.LeanCheck
  , module Test.LeanCheck.Utils
  , module Test.Speculate
  , module Data.Express.Fixtures
  , module Test.ListableExpr

  -- * Test reporting
  , reportTests
  , getMaxTestsFromArgs
  , mainTest
  , printLines

  -- * Test types
  , listThyInefficient

  , Thyght (..)
  , Equation (..)

  -- * Test fixtures
  , foo, goo

  -- * Enumerate expressions
  , expressionsT
  )
where

import Test.LeanCheck
import Test.LeanCheck.Utils hiding (comparison)

import Data.Express.Fixtures hiding (compose)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (elemIndices)

import Test.Speculate hiding (getArgs)
import Test.Speculate.Reason
import Test.Speculate.Reason.Order

import Data.List (sort)

import Test.Speculate.Utils

import Test.ListableExpr


-- test reporting --

reportTests :: [Bool] -> IO ()
reportTests tests  =  case elemIndices False tests of
  [] -> putStrLn "+++ Tests passed!"
  is -> putStrLn ("*** Failed tests:" ++ show is) >> exitFailure

getMaxTestsFromArgs :: Int -> IO Int
getMaxTestsFromArgs n = do
  as <- getArgs
  return $ case as of
             (s:_) -> read s
             _     -> n

mainTest :: (Int -> [Bool]) -> Int -> IO ()
mainTest tests n' = do
  n <- getMaxTestsFromArgs n'
  reportTests (tests n)

printLines :: Show a => [a] -> IO ()
printLines = putStrLn . unlines . map show


-- test fixtures --

foo :: Expr -> Expr
foo = (value "f" (undefined :: Int -> Int) :$)

goo :: Expr -> Expr
goo = (value "g" (undefined :: Int -> Int) :$)


-- test types --

data Rule = Rule Expr Expr deriving (Show, Eq, Ord)
data Equation = Equation Expr Expr deriving (Show, Eq, Ord)
newtype RuleSet = RuleSet [(Expr,Expr)] deriving Show
newtype EquationSet = EquationSet [(Expr,Expr)] deriving Show
newtype Thyght = Thyght { unThyght :: Thy } deriving Show


-- Listable instances --

-- beware: enumerating beyond 600 values will  make this very slow as it is
-- very hard to satisfy canonicalEqn and ->-.  In practice, this should not be a
-- problem as we enumerate far less than that when enerating 'Thy's.
instance Listable Rule where
  tiers = (`ofWeight` 0)
        . filterT (\(Rule e1 e2) -> canonicalRule (e1,e2) && e1 ->- e2)
        . mapT (uncurry Rule . orientRule)
        . filterT (uncurry (<))
        . mapT unSameTypeE
        $ tiers
    where
    (->-) = canReduceTo emptyThy
    orientRule (e1,e2) | e1 ->- e2 = (e1,e2)
                       | otherwise = (e2,e1)

instance Listable Equation where
  tiers = (`ofWeight` 0)
        . mapT (uncurry Equation)
        . filterT (canonicalEqn emptyThy)
        . mapT orientEqn
        . filterT (uncurry (<=))
        . mapT unSameTypeE
        $ tiers
    where
    orientEqn (e1,e2) | e1 `compare` e2 == LT = (e2,e1)
                      | otherwise             = (e1,e2)

instance Listable RuleSet where
  tiers = setCons (RuleSet . map unRule) `ofWeight` 0
    where
    unRule (Rule e1 e2) = (e1,e2)

instance Listable EquationSet where
  tiers = setCons (EquationSet . map unEquation) `ofWeight` 0
    where
    unEquation (Equation e1 e2) = (e1,e2)

instance Listable Thy where
  tiers = concatMapT expandCanReduceTo
        $ concatMapT expandClosureLimit
        $ concatMapT expandKeepE
        $ cons2 (\(RuleSet rs) (EquationSet eqs)
                   -> emptyThy { rules     = sort rs
                               , equations = sort eqs })

instance Listable Thyght where
  tiers = mapT Thyght
        $ concatMapT expandCanReduceTo
        $ concatMapT expandClosureLimit
        $ mapT defaultKeep
        $ cons2 (\(RuleSet rs) (EquationSet eqs)
                   -> emptyThy { rules     = sort rs
                               , equations = sort eqs })

expandKeepE :: Thy -> [[Thy]]
expandKeepE thy = cons0 thy
               \/ cons0 thy {keepE = keepUpToLength (maxLen + 0)} `ofWeight` 1
               \/ cons0 thy {keepE = keepUpToLength (maxLen + 1)} `ofWeight` 2
               \/ cons0 thy {keepE = keepUpToLength (maxLen + 2)} `ofWeight` 4
               \/ cons0 thy {keepE = keepUpToLength (maxLen + 3)} `ofWeight` 6
               \/ cons0 thy {keepE = keepUpToLength (maxLen + 4)} `ofWeight` 8
  where
  maxLen = maximum . (0:) . map size . catPairs $ equations thy ++ rules thy

expandClosureLimit :: Thy -> [[Thy]]
expandClosureLimit thy = cons0 thy {closureLimit = 3}
                      \/ cons0 thy {closureLimit = 0} `ofWeight` 1
                      \/ cons0 thy {closureLimit = 2} `ofWeight` 2
                      \/ cons0 thy {closureLimit = 1} `ofWeight` 3

-- TODO: make Listable Thy enumeration complete w.r.t: canReduceTo
-- for a complete version, Listable Rule will have to be transformed on a
-- higher order function that take canReduceTo.  (harder to maintain)
expandCanReduceTo :: Thy -> [[Thy]]
expandCanReduceTo thy = cons0 thy
                     \/ if all (uncurry (|>|)) (rules thy)
                          then cons0 thy {canReduceTo = (|>|)} `ofWeight` 1
                          else []
-- FIXME: KBO is broken ATM:
--                   \/ if all (uncurry ( >|)) (rules thy)
--                        then cons0 thy {canReduceTo = ( >|)} `ofWeight` 2
--                        else []

listThyInefficient :: [Thy]
listThyInefficient = concat
                   . concatMapT expandCanReduceTo
                   . concatMapT expandClosureLimit
                   . concatMapT expandKeepE
                   $ cons2 (\(SameTypedPairsE rs) (SameTypedPairsE eqs)
                              -> emptyThy { rules     = sort rs
                                          , equations = sort eqs
                                          }) `suchThat` okThy

-- Quick and Dirty!
instance Show Thy where
  show Thy { rules = rs
           , equations = eqs
           , canReduceTo = (->-)
           , closureLimit = cl
           , keepE = keep
           }
    = "Thy { rules = "
   ++ drop 14 (indent 14 . listLines $ map showEquation rs)
   ++ "    , equations = "
   ++ drop 18 (indent 18 . listLines $ map showEquation eqs)
   ++ "    , canReduceTo = " ++ showCanReduceTo (->-) ++ "\n"
   ++ "    , closureLimit = " ++ show cl ++ "\n"
   ++ "    , keepE = " ++ showKeepE keep ++ "\n"
   ++ "    }"
    where
    showEquation (e1,e2) = showExpr e1 ++ " == " ++ showExpr e2
    listLines [] = "[]"
    listLines ss = '[':(tail . unlines $ map (", " ++) ss) ++ "]"
    showCanReduceTo (->-) | holds 1000 $ (->-) ==== (|>|) = "(|>|)"
                          | holds 1000 $ (->-) ==== ( >|) =  "(>|)"
                          | holds 1000 $ (->-) ==== (|> ) = "(|>)"
                          | otherwise = "(??)"
    showKeepE keep | holds 1000 $ keep === const True = "const True"
                   | holds 1000 $ keep === keepUpToLength 0 = "keepUpToLength 0"
                   | holds 1000 $ keep === keepUpToLength 1 = "keepUpToLength 1"
                   | holds 1000 $ keep === keepUpToLength 2 = "keepUpToLength 2"
                   | holds 1000 $ keep === keepUpToLength 3 = "keepUpToLength 3"
                   | holds 1000 $ keep === keepUpToLength 4 = "keepUpToLength 4"
                   | holds 1000 $ keep === keepUpToLength 5 = "keepUpToLength 5"
                   | holds 1000 $ keep === keepUpToLength 6 = "keepUpToLength 6"
                   | holds 1000 $ keep === keepUpToLength 7 = "keepUpToLength 7"
                   | holds 1000 $ keep === keepUpToLength 8 = "keepUpToLength 8"
                   | holds 1000 $ keep === keepUpToLength 9 = "keepUpToLength 9"
                   | otherwise = "\\e -> ??"

expressionsT :: [Expr] -> [[Expr]]
expressionsT ds = [ds] \/ productMaybeWith ($$) es es `addWeight` 1
  where
  es = expressionsT ds
-- TODO: maybe use expressionsT as the main function to generate Exprs.
-- By using it, I speculate a 20% increase in runtime.  But the code will
-- certainly be smaller and easier to maintain.
