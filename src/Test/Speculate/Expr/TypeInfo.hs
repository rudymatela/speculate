module Test.Speculate.Expr.TypeInfo
  ( Instances
  , Instance (..)
  , TypeRep

  -- * Smart constructors
  , ins

  -- * Queries on Instances1 lists
  , findInfo
  , names
  , eqE,      isEq
  , leE, ltE, isOrd
  ,           isEqOrd
  , tiersE,   isListable

  -- * Type info for standard Haskell types
  , preludeInstances

  -- * Does not belong here?
  , defNames

  , boolTy
  , mkEqnTy
  )
where

import Test.Speculate.Expr.Core
import Test.Speculate.Expr.Match
import Test.Speculate.Utils hiding (ord)
import Test.LeanCheck
import Test.LeanCheck.Utils hiding (comparison)
import Test.LeanCheck.Error (errorToFalse)
import Data.Dynamic

import Data.Maybe (isJust,fromMaybe,listToMaybe,catMaybes)
import Data.List (find,(\\))


-- | Type information needed to Speculate expressions (single type / single class).
data Instance = Eq TypeRep Expr
              | Ord TypeRep Expr Expr
              | Listable TypeRep [[Expr]]
              | Names TypeRep [String]

-- | Type information needed to Speculate expressions.
type Instances = [Instance]

-- | Usage: @typeInfo (undefined :: Type) "x"@
ins1 :: (Typeable a, Listable a, Show a, Eq a, Ord a)
          => String -> a -> Instances
ins1 n x = eq x ++ ord x ++ listable x ++ name n x

ins :: (Typeable a, Listable a, Show a, Eq a, Ord a)
    => String -> a -> Instances
ins n x = concat
  [    x      / n

  ,   [x]     / n ++ "s"
  ,  [[x]]    / n ++ "ss"
--, [[[x]]]   / n ++ "ss"

  , (x,x)     / n ++ m
  , (x,x,x)   / n ++ m ++ o
--, (x,x,x,x) / n ++ m ++ o ++ p

  , [(x,x)]   / n ++ m ++ "s"
--, [(x,x,x)] / n ++ m ++ o ++ "ss"

--, (x,[x])   / n ++ m ++ "s"
--, ([x],x)   / n ++ "s" ++ m
--, ([x],[x]) / n ++ "s" ++ m ++ "s"
--, (x,(x,x)) / n ++ m ++ o
--, ((x,x),x) / n ++ m ++ o

  , (mayb x)  / "m" ++ n ++ "1"
--, (eith x x) / "e" ++ n ++ o ++ "1"
  ]
  where
  (/) :: (Typeable a, Listable a, Show a, Eq a, Ord a)
      => a -> String -> Instances -- monomorphism restriction strikes again
  (/) = flip ins1
  infixr 0 /
  m = namesFromTemplate n !! 1
  o = namesFromTemplate m !! 1
  p = namesFromTemplate o !! 1
-- NOTE: the function typeInfoN is not perfect: it won't help produce types
-- combining different sub-types, like for example: (Bool,Int).  But it is
-- way better than the original version in which I had to explictly define
-- everything.  A definitive solution is still to be thought of.
-- NOTE: see related TODO on the definition of basicInstances

eq :: (Typeable a, Eq a) => a -> Instances
eq x = [Eq (typeOf x) . constant "==" $ (errorToFalse .: (==)) -:> x]

ord :: (Typeable a, Ord a) => a -> Instances
ord x = [Ord (typeOf x) (constant "<=" $ (errorToFalse .: (<=)) -:> x)
                        (constant "<"  $ (errorToFalse .: (<))  -:> x)]

listable :: (Typeable a, Show a, Listable a) => a -> Instances
listable x = [Listable (typeOf x) . mapT showConstant $ tiers `asTypeOf` [[x]]]

name :: Typeable a => String -> a -> Instances
name n x = [Names (typeOf x) (namesFromTemplate n)]

-- TODO: make types consistent!  add isOrdE and isEqE?
isOrd :: Instances -> Expr -> Bool
isOrd ti = isJust . ltE ti . typ

isEq :: Instances -> Expr -> Bool
isEq ti = isJust . eqE ti . typ

isEqOrd :: Instances -> Expr -> Bool
isEqOrd ti e = isOrd ti e && isEq ti e

isListable :: Instances -> TypeRep -> Bool
isListable ti t = isJust $ findInfo m ti
  where
  m (Listable t' ts) | t' == t = Just ts
  m _                          = Nothing

-- TODO: implement above using something similar to the following
-- isComparable ti = isJust . (`findInfo` ti) . typ

findInfo :: (Instance -> Maybe a) -> Instances -> Maybe a
findInfo may = listToMaybe . catMaybes . map may

findInfoOr :: a -> (Instance -> Maybe a) -> Instances -> a
findInfoOr def may = fromMaybe def . findInfo may

names :: Instances -> TypeRep -> [String]
names ti t = findInfoOr defNames m ti
  where
  m (Names t' ns) | t == t' = Just ns
  m _                       = Nothing

tiersE :: Instances -> TypeRep -> [[Expr]]
tiersE ti t = findInfoOr (error $ "could not find Listable " ++ show t) m ti
  where
  m (Listable t' ts) | t == t' = Just ts
  m _                          = Nothing

eqE :: Instances -> TypeRep -> Maybe Expr
eqE ti t = findInfo m ti
  where
  m (Eq t' eq) | t == t' = Just eq
  m _                    = Nothing

ltE :: Instances -> TypeRep -> Maybe Expr
ltE ti t = findInfo m ti
  where
  m (Ord t' _ lt) | t == t' = Just lt
  m _                       = Nothing

leE :: Instances -> TypeRep -> Maybe Expr
leE ti t = findInfo m ti
  where
  m (Ord t' le _) | t == t' = Just le
  m _                       = Nothing

-- TODO: include *ALL* prelude types on basicInstances
preludeInstances :: Instances
preludeInstances = concat
  [ ins1 "x"  (undefined :: ())
  , ins1 "xs" (undefined :: [()])

  , ins "p" (undefined :: Bool)

  , ins "x" (undefined :: Int)
--, ins "x" (undefined :: Word)
  , ins "x" (undefined :: Integer)

  , ins "o" (undefined :: Ordering)
  , ins "c" (undefined :: Char)

  , ins "q" (undefined :: Rational)
  , ins "f" (undefined :: Float)
  , ins "f" (undefined :: Double)

-- TODO: uncomment the following and investigate why compilation takes so long
--, ins "x" (undefined :: Int1)
--, ins "x" (undefined :: Int2)
--, ins "x" (undefined :: Int3)
--, ins "x" (undefined :: Int4)
--, ins "x" (undefined :: Word1)
  , ins "x" (undefined :: Word2)
--, ins "x" (undefined :: Word3)
--, ins "x" (undefined :: Word4)
--, ins "x" (undefined :: Nat1)
--, ins "x" (undefined :: Nat2)
--, ins "x" (undefined :: Nat3)
--, ins "x" (undefined :: Nat4)
--, ins "x" (undefined :: Nat5)
--, ins "x" (undefined :: Nat6)
--, ins "x" (undefined :: Nat7)
  ]
-- WHOA!  Have I discovered a "bug" in GHC?  adding to many type compositions
-- on ins and types on preludeInstances makes compilation of this module
-- *really* slow: it takes a whopping 2 minutes!
-- (the above report is using -O2, I have not tested without optimizations).


defNames :: [String]
defNames = namesFromTemplate "x"
