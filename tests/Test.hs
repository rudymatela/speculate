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
  , module Data.Haexpress.Fixtures
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

  -- * Functions and values encoded as 'Expr' or functions of Exprs
  -- | Terminal values are named;
  --   Variables are duplicated;
  --   Functions are primed;
  --   Operators are surrounded by dashes.

  -- ** Integers
  , (.-.)
  , ii'
  , ff, gg
  , ff2, hh2, hh3, hh4, hh5, hh6, hh7
  , succ'

  , succE
  , minusE
  , commaE
  , ffE
  , ggE

  -- ** Booleans
  , rr

  -- ** Characters
  , aa, bb
  , space, lineBreak
  , emptyString

  -- ** Lists (of Inteters)
  , ll

  , appendE

  -- ** Typereps
  , charTy

  -- ** checks for types
  , intE
  , charE
  , boolE
  , listE

  -- ** Enumerate expressions
  , expressionsT
  )
where

import Test.LeanCheck
import Test.LeanCheck.Utils hiding (comparison)

import Data.Haexpress.Fixtures hiding
  (ff, ffE, gg, ggE) -- as we define them as constants here

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (elemIndices)

import Test.Speculate hiding (getArgs)
import Test.Speculate.Expr
import Test.Speculate.Reason
import Test.Speculate.Reason.Order

import Data.List (sort)

import Test.Speculate.Utils

import Test.ListableExpr

reportTests :: [Bool] -> IO ()
reportTests tests =
  case elemIndices False tests of
    [] -> putStrLn "+++ Tests passed!"
    is -> do putStrLn ("*** Failed tests:" ++ show is)
             exitFailure

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

succ' :: Expr -> Expr
succ' = (succE :$)

succE :: Expr
succE = constant "succ" ((1+) :: Int -> Int)

(.-.) :: Expr -> Expr -> Expr
e1 .-. e2 = minusE :$ e1 :$ e2

minusE :: Expr
minusE = constant "-" ((-) :: Int -> Int -> Int)

commaE :: Expr
commaE = constant "," ((,) :: Int -> Int -> (Int,Int))

ii' :: Expr
ii' = var "i'" int

ff :: Expr -> Expr
ff = (ffE :$)

ffE :: Expr
ffE = constant "f" (undefined :: Int -> Int)

gg :: Expr -> Expr
gg = (ggE :$)

ggE :: Expr
ggE = constant "g" (undefined :: Int -> Int)

ff2 :: Expr -> Expr -> Expr
ff2 e1 e2 = ffE :$ e1 :$ e2
  where ffE = constant "f" (undefined :: Int -> Int -> Int)

hh2 :: Expr -> Expr -> Expr
hh2 e1 e2 = hhE :$ e1 :$ e2
  where hhE = constant "h" (undefined :: Int -> Int -> Int)

hh3 :: Expr -> Expr -> Expr -> Expr
hh3 e1 e2 e3 = hhE :$ e1 :$ e2 :$ e3
  where hhE = constant "h" (undefined :: Int -> Int -> Int -> Int)

hh4 :: Expr -> Expr -> Expr -> Expr -> Expr
hh4 e1 e2 e3 e4 = hhE :$ e1 :$ e2 :$ e3 :$ e4
  where hhE = constant "h" (undefined :: Int -> Int -> Int -> Int -> Int)

hh5 :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr
hh5 e1 e2 e3 e4 e5 = hhE :$ e1 :$ e2 :$ e3 :$ e4 :$ e5
  where hhE = constant "h" (undefined :: Int -> Int -> Int -> Int -> Int -> Int)

hh6 :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
hh6 e1 e2 e3 e4 e5 e6 = hhE :$ e1 :$ e2 :$ e3 :$ e4 :$ e5 :$ e6
  where hhE = constant "h" (undefined :: Int -> Int -> Int -> Int -> Int -> Int -> Int)

hh7 :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
hh7 e1 e2 e3 e4 e5 e6 e7 = hhE :$ e1 :$ e2 :$ e3 :$ e4 :$ e5 :$ e6 :$ e7
  where hhE = constant "h" (undefined :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int)

-- unification (hh5 yy zz (ff2 ii ii) (ff2 jj jj) kk) (hh5 (ff2 xx xx) (ff2 yy yy) jj kk zz)

-- unification (hh7 yy zz xx' (ff2 ii ii) (ff2 jj jj) (ff2 kk kk) ii')
--             (hh7 (ff2 xx xx) (ff2 yy yy) (ff2 zz zz) jj kk ii' xx')

rr :: Expr -- ar, I'm a pirate
rr = var "r" bool


aa :: Expr -- a, the character, not a variable
aa = showConstant 'a'

bb :: Expr -- bee, the character, not a variable
bb = showConstant 'b'

space :: Expr -- space, the character
space = showConstant ' '

lineBreak :: Expr -- lineBreak, the character
lineBreak = showConstant '\n'

emptyString :: Expr
emptyString = showConstant ""


ll :: Expr
ll = showConstant ([] :: [Int])

appendE :: Expr
appendE = constant "++" ((++) :: [Int] -> [Int] -> [Int])


-- boolTy already exported by Speculate.Instance

charTy :: TypeRep
charTy = typeOf char

listTy :: TypeRep
listTy = typeOf [int]

intE :: Expr -> Bool
intE e = typ e == intTy

boolE :: Expr -> Bool
boolE e = typ e == boolTy

charE :: Expr -> Bool
charE e = typ e == charTy

listE :: Expr -> Bool
listE e = typ e == listTy

data Rule = Rule Expr Expr deriving (Show, Eq, Ord)
data Equation = Equation Expr Expr deriving (Show, Eq, Ord)

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

newtype RuleSet = RuleSet [(Expr,Expr)] deriving Show
newtype EquationSet = EquationSet [(Expr,Expr)] deriving Show

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

newtype Thyght = Thyght { unThyght :: Thy } deriving Show

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
