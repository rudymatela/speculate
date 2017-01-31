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

  -- * Test reporting
  , reportTests
  , getMaxTestsFromArgs
  , mainTest
  , printLines

  -- * Properties
  , tiersExprTypeCorrect

  , listThyInefficient

  , IntE  (..)
  , BoolE (..)
  , CharE (..)
  , ListE (..)
  , SameTypeE (..)
  , unSameTypeE
  , SameTypedPairsE (..)
  , Thyght (..)
  , Equation (..)

  -- * Functions and values encoded as 'Expr' or functions of Exprs
  -- | Terminal values are named;
  --   Variables are duplicated;
  --   Functions are primed;
  --   Operators are surrounded by dashes.

  -- ** Integers
  , zero, one
  , xx, yy, zz, xx'
  , id', abs'
  , (-+-), (-*-), (.-.)
  , ii, jj, kk, ii'
  , negate'
  , ff, gg
  , succ'

  , idE
  , absE
  , succE
  , negateE
  , plusE
  , timesE
  , minusE

  -- ** Booleans
  , true, false
  , pp, qq, rr
  , not', (-&&-), (-||-), (-==>-)
  , (-==-), (-<=-), (-<-)
  , odd', even'

  -- ** Characters
  , aa
  , cc, dd
  , ord'
  , ordE

  -- ** Lists (of Inteters)
  , ll
  , xxs, yys
  , (-:-), (-++-)
  , insert', elem', sort'

  , consE, appendE

  -- ** Typereps
  , intTy
  , charTy

  -- ** checks for types
  , intE
  , charE
  , boolE

  -- ** Unamed holes
  , i_
  , c_
  , b_

  -- ** Dummy
  , expr

  -- ** Enumerate expressions
  , expressionsT
  )
where

import Test.LeanCheck
import Test.LeanCheck.Tiers
import Test.LeanCheck.Utils hiding (comparison)

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (elemIndices)

import Test.Speculate hiding (getArgs, true, false)
import Test.Speculate.Reason
import Test.Speculate.Reason.Order

import Data.Char (ord)
import Data.Dynamic
import Data.Function (on)
import Data.List as L (sort,insert)

import Test.Speculate.Utils


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

-- | This will not enumerate all possible 'Expr's, as that is impossible.
--   But eventually, a rather a nice subset of it, with Integers, Booleans,
--   Chars and lists of Integers.
instance Listable Expr where
  tiers = cons1 unIntE
       \/ cons1 unBoolE
       \/ cons1 unCharE
       \/ cons1 unListE `addWeight` 1
       \/ cons1 unFunE  `addWeight` 1

tiersExprTypeCorrect :: Int -> Bool
tiersExprTypeCorrect n = all typeCorrect $ take n (list :: [Expr])

-- Not a particularly efficient implementation.  If performance ever becomes an
-- issue, declare something like:
--
-- > tiersIntE = ...
-- >          \/ mapT ord tiersCharE
-- >          \/ ...
-- >   where
-- >   cons1 c = mapT c tiersIntE
-- >   cons2 c = mapT ...

data IntE  = IntE  { unIntE  :: Expr } deriving Show
data BoolE = BoolE { unBoolE :: Expr } deriving Show
data CharE = CharE { unCharE :: Expr } deriving Show
data ListE = ListE { unListE :: Expr } deriving Show
data FunE  = FunE  { unFunE  :: Expr } deriving Show

consI :: (Expr -> a) -> [[a]]; consI f = cons1 (f . unIntE)
consB :: (Expr -> a) -> [[a]]; consB f = cons1 (f . unBoolE)
consC :: (Expr -> a) -> [[a]]; consC f = cons1 (f . unCharE)
consL :: (Expr -> a) -> [[a]]; consL f = cons1 (f . unListE)
consF :: (Expr -> a) -> [[a]]; consF f = cons1 (f . unFunE)
consII :: (Expr -> Expr -> a) -> [[a]]; consII o = cons2 (o `on` unIntE)
consBB :: (Expr -> Expr -> a) -> [[a]]; consBB o = cons2 (o `on` unBoolE)
consLL :: (Expr -> Expr -> a) -> [[a]]; consLL o = cons2 (o `on` unListE)
consIL :: (Expr -> Expr -> a) -> [[a]]; consIL o = cons2 (\(IntE x) (ListE xs) -> x `o` xs)

instance Listable IntE where
  tiers = mapT IntE $ cons0 zero   `addWeight` 1
                   \/ cons0 one    `addWeight` 2
                   \/ cons0 i_
                   \/ cons0 xx
                   \/ cons0 yy     `addWeight` 1
                   \/ cons0 zz     `addWeight` 2
                   \/ consI id'
                   \/ consI abs'   `addWeight` 1
                   \/ consII (-+-)
                   \/ consII (-*-) `addWeight` 1
                   \/ consC ord'   `addWeight` 2

instance Listable BoolE where
  tiers = mapT BoolE $ cons0 true   `addWeight` 1
                    \/ cons0 false  `addWeight` 1
                    \/ cons0 b_
                    \/ cons0 pp
                    \/ cons0 qq     `addWeight` 1
                    \/ cons0 rr     `addWeight` 2
                    \/ consB not'
                    \/ consBB (-&&-)  `addWeight` 1
                    \/ consBB (-||-)  `addWeight` 2
                    \/ consBB (-==>-) `addWeight` 3
                    \/ maybeCons1 (uncurry (equation     basicTypeInfo) . unSameTypeE) `addWeight` 3
                    \/ maybeCons1 (uncurry (comparisonL  basicTypeInfo) . unSameTypeE) `addWeight` 4
                    \/ maybeCons1 (uncurry (comparisonLE basicTypeInfo) . unSameTypeE) `addWeight` 4
                    \/ consI odd'   `addWeight` 1
                    \/ consI even'  `addWeight` 1
                    \/ consIL elem' `addWeight` 4

instance Listable CharE where
  tiers = mapT CharE $ cons0 aa   `addWeight` 1
                    \/ cons0 c_
                    \/ cons0 cc
                    \/ cons0 dd   `addWeight` 1

instance Listable ListE where
  tiers = mapT ListE $ cons0 ll
                    \/ cons0 xxs
                    \/ cons0 yys        `addWeight` 1
                    \/ consIL (-:-)
                    \/ consLL (-++-)    `addWeight` 1
                    \/ consIL (insert') `addWeight` 2
                    \/ consL  (sort')   `addWeight` 2

instance Listable FunE where
  list = map FunE
       [ idE
       , plusE
       , appendE
       , ordE
       , consE
       , absE
       , timesE
       , negateE
       , succE
       ]

data SameTypeE = SameTypeE Expr Expr deriving Show

unSameTypeE :: SameTypeE -> (Expr,Expr)
unSameTypeE (SameTypeE e1 e2) = (e1,e2)

instance Listable SameTypeE where
  tiers = cons1 (\(IntE  e1, IntE  e2) -> SameTypeE e1 e2) `ofWeight` 0
       \/ cons1 (\(BoolE e1, BoolE e2) -> SameTypeE e1 e2) `ofWeight` 0
       \/ cons1 (\(CharE e1, CharE e2) -> SameTypeE e1 e2) `ofWeight` 0
       \/ cons1 (\(ListE e1, ListE e2) -> SameTypeE e1 e2) `ofWeight` 0
       \/ cons1 (\(FunE  e1, FunE  e2) -> SameTypeE e1 e2) `ofWeight` 0
          `suchThat` (\(SameTypeE e1 e2) -> typ e1 == typ e2) -- for func, manual

data SameTypedPairsE = SameTypedPairsE [(Expr,Expr)] deriving Show

instance Listable SameTypedPairsE where
  tiers = cons1 (SameTypedPairsE . map unSameTypeE) `ofWeight` 0


zero :: Expr
zero = showConstant (0 :: Int)

one :: Expr
one = showConstant (1 :: Int)

xx :: Expr -- ex
xx = var "x" int

yy :: Expr -- wye
yy = var "y" int

zz :: Expr -- zed
zz = var "z" int

xx' :: Expr -- ex prime
xx' = var "x'" int

id' :: Expr -> Expr
id' = (idE :$)

idE :: Expr
idE = constant "id" (id :: Int -> Int)

abs' :: Expr -> Expr
abs' = (absE :$)

absE :: Expr
absE = constant "abs" (abs :: Int -> Int)

negate' :: Expr -> Expr
negate' = (negateE :$)

negateE :: Expr
negateE = constant "negate" (negate :: Int -> Int)

succ' :: Expr -> Expr
succ' = (succE :$)

succE :: Expr
succE = constant "succ" ((1+) :: Int -> Int)

(-+-) :: Expr -> Expr -> Expr
e1 -+- e2 = plusE :$ e1 :$ e2
infixl 6 -+-

plusE :: Expr
plusE = constant "+" ((+) :: Int -> Int -> Int)

(-*-) :: Expr -> Expr -> Expr
e1 -*- e2 = timesE :$ e1 :$ e2

timesE :: Expr
timesE = constant "*" ((*) :: Int -> Int -> Int)

(.-.) :: Expr -> Expr -> Expr
e1 .-. e2 = minusE :$ e1 :$ e2

minusE :: Expr
minusE = constant "-" ((-) :: Int -> Int -> Int)

ii :: Expr
ii = var "i" int

jj :: Expr
jj = var "j" int

kk :: Expr
kk = var "k" int

ii' :: Expr
ii' = var "i'" int

ff :: Expr -> Expr
ff = (ffE :$) where ffE = constant "f" (undefined :: Int -> Int)

gg :: Expr -> Expr
gg = (ggE :$) where ggE = constant "g" (undefined :: Int -> Int)


true :: Expr
true = showConstant (True :: Bool)

false :: Expr
false = showConstant (False :: Bool)

pp :: Expr -- pee
pp = var "p" bool

qq :: Expr -- cue
qq = var "q" bool

rr :: Expr -- ar, I'm a pirate
rr = var "r" bool

not' :: Expr -> Expr
not' = (notE :$) where notE = constant "not" not

(-&&-) :: Expr -> Expr -> Expr
e1 -&&- e2 = andE :$ e1 :$ e2 where andE = constant "&&" (&&)
infixr 3 -&&-

(-||-) :: Expr -> Expr -> Expr
e1 -||- e2 = orE :$ e1 :$ e2 where orE = constant "||" (||)
infixr 2 -||-

(-==>-) :: Expr -> Expr -> Expr
e1 -==>- e2 = impliesE :$ e1 :$ e2 where impliesE = constant "==>" (==>)
infixr 0 -==>-

(-==-) :: Expr -> Expr -> Expr
e1 -==- e2 =
  case equation basicTypeInfo e1 e2 of
    Nothing -> error $ "(-==-): cannot equate " ++ show e1 ++ " and " ++ show e2
    Just eq -> eq
infix 4 -==-

(-<=-) :: Expr -> Expr -> Expr
e1 -<=- e2 =
  case comparisonLE basicTypeInfo e1 e2 of
    Nothing -> error $ "(-<=-): cannot lessEq " ++ show e1 ++ " and " ++ show e2
    Just eq -> eq
infix 4 -<=-

(-<-) :: Expr -> Expr -> Expr
e1 -<- e2 =
  case comparisonL basicTypeInfo e1 e2 of
    Nothing -> error $ "(-<-): cannot less " ++ show e1 ++ " and " ++ show e2
    Just eq -> eq
infix 4 -<-

odd' :: Expr -> Expr
odd' = (oddE :$) where oddE = constant "odd" (odd :: Int -> Bool)

even' :: Expr -> Expr
even' = (evenE :$) where evenE = constant "even" (even :: Int -> Bool)


aa :: Expr -- a, the character, not variable
aa = showConstant 'a'

cc :: Expr -- cee, a variable character
cc = var "c" char

dd :: Expr -- dee, a variable character
dd = var "d" char

ord' :: Expr -> Expr
ord' = (ordE :$)

ordE :: Expr
ordE = constant "ord" ord


ll :: Expr
ll = showConstant ([] :: [Int])

xxs :: Expr -- exes
xxs = var "xs" [int]

yys :: Expr -- wyes
yys = var "ys" [int]

(-:-) :: Expr -> Expr -> Expr
e1 -:- e2 = (consE :$ e1 :$ e2)
infixr 5 -:-

consE :: Expr
consE = constant ":" ((:) :: Int -> [Int] -> [Int])

(-++-) :: Expr -> Expr -> Expr
e1 -++- e2 = appendE :$ e1 :$ e2
infixr 5 -++-

appendE :: Expr
appendE = constant "++" ((++) :: [Int] -> [Int] -> [Int])

insert' :: Expr -> Expr -> Expr
insert' ex exs = insertE :$ ex :$ exs where insertE = constant "insert" (L.insert :: Int -> [Int] -> [Int])

elem' :: Expr -> Expr -> Expr
elem' ex exs = elemE :$ ex :$ exs where elemE = constant "elem" (elem :: Int -> [Int] -> Bool)

sort' :: Expr -> Expr
sort' exs = sortE :$ exs where sortE = constant "sort" (sort :: [Int] -> [Int])

-- boolTy already exported by Speculate.TypeInfo

intTy :: TypeRep
intTy = typeOf int

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

i_ :: Expr
i_ = hole int

c_ :: Expr
c_ = hole char

b_ :: Expr
b_ = hole bool

xs_ :: Expr
xs_ = hole [int]

-- | Dummy expr value, for use in type binding
expr :: Expr
expr = undefined


data Rule = Rule Expr Expr deriving (Show, Eq, Ord)
data Equation = Equation Expr Expr deriving (Show, Eq, Ord)

unEquation :: Equation -> (Expr,Expr)
unEquation (Equation e1 e2) = (e1,e2)

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
        . filterT canonicalEqn
        . mapT orientEqn
        . filterT (uncurry (<=))
        . mapT unSameTypeE
        $ tiers
    where
    orientEqn (e1,e2) | e1 `compareComplexity` e2 == LT = (e2,e1)
                      | otherwise                       = (e1,e2)

data RuleSet = RuleSet [(Expr,Expr)] deriving Show
data EquationSet = EquationSet [(Expr,Expr)] deriving Show

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

data Thyght = Thyght { unThyght :: Thy } deriving Show

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
  maxLen = maximum . map lengthE . catPairs $ equations thy ++ rules thy

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
                     \/ if all (uncurry ( >|)) (rules thy)
                          then cons0 thy {canReduceTo = ( >|)} `ofWeight` 2
                          else []

listThyInefficient :: [Thy]
listThyInefficient = concat
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
