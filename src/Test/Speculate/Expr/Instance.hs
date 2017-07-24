{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC <= 7.8
-- |
-- Module      : Test.Speculate.Expr.Instance
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Typeclass instance information.
module Test.Speculate.Expr.Instance
  ( Instances
  , Instance (..)
  , TypeRep

  -- * Smart constructors
  , ins
  , eq,       eqWith
  , ord,      ordWith
  , eqOrd
  , listable, listableWith
  , name

  -- * Queries on Instances
  , instanceType
  , findInfo
  , names
  , eqE,      isEq,       isEqE
  , leE, ltE, isOrd,      isOrdE
  ,           isEqOrd,    isEqOrdE
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

import Data.Maybe (isJust,fromMaybe,listToMaybe,catMaybes,mapMaybe)
import Data.List (find,(\\))
import Data.Monoid ((<>))


-- | Type information needed to Speculate expressions (single type / single class).
data Instance = Instance String TypeRep [Expr]
  deriving Show

instance Eq Instance where
  Instance s1 t1 _ == Instance s2 t2 _  =  s1 == s2 && t1 == t2

instance Ord Instance where
  Instance s1 t1 _ `compare` Instance s2 t2 _  =  s1 `compare` s2 <> t1 `compare` t2


-- | Type information needed to Speculate expressions.
type Instances = [Instance]

instanceType :: Instance -> TypeRep
instanceType (Instance _ t _)  =  t

-- | Usage: @ins1 "x" (undefined :: Type)@
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

  , mayb x    / "m" ++ n ++ "1"
--, eith x x  / "e" ++ n ++ o ++ "1"
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
eq x = eqWith $ (==) -:> x

ord :: (Typeable a, Ord a) => a -> Instances
ord x = ordWith $ (<=) -:> x

eqOrd :: (Typeable a, Eq a, Ord a) => a -> Instances
eqOrd x = eq x ++ ord x

listable :: (Typeable a, Show a, Listable a) => a -> Instances
listable x = listableWith $ tiers `asTypeOf` [[x]]

name :: Typeable a => String -> a -> Instances
name n x = [ Instance "Names" (typeOf x)
               [constant "names" $ namesFromTemplate n] ]

eqWith :: (Typeable a, Eq a) => (a -> a -> Bool) -> Instances
eqWith (==) = [ Instance "Eq" (typeOf $ arg (==))
                  [constant "==" $ errorToFalse .: (==)] ]
  where
  arg :: (a -> b) -> a
  arg _ = undefined

ordWith :: (Typeable a, Ord a) => (a -> a -> Bool) -> Instances
ordWith (<=) = [ Instance "Ord" (typeOf $ arg (<=))
                   [ constant "<=" $ errorToFalse .: (<=)
                   , constant "<"  $ (errorToFalse . not) .: flip (<=) ] ]
  where
  arg :: (a -> b) -> a
  arg _ = undefined

listableWith :: (Typeable a, Show a) => [[a]] -> Instances
listableWith xss = [ Instance "Listable" (typeOf $ head $ head xss)
                       [constant "tiers" $ mapT showConstant xss] ]

isEq :: Instances -> TypeRep -> Bool
isEq ti = isJust . eqE ti

isOrd :: Instances -> TypeRep -> Bool
isOrd ti = isJust . ltE ti

isEqOrd :: Instances -> TypeRep -> Bool
isEqOrd ti t = isOrd ti t && isEq ti t

isEqE :: Instances -> Expr -> Bool
isEqE ti = isEq ti . typ

isOrdE :: Instances -> Expr -> Bool
isOrdE ti = isOrd ti . typ

isEqOrdE :: Instances -> Expr -> Bool
isEqOrdE ti = isEqOrd ti . typ

isListable :: Instances -> TypeRep -> Bool
isListable ti t = isJust $ findInfo m ti
  where
  m (Instance "Listable" t' ts) | t' == t = Just ts
  m _                                     = Nothing

-- TODO: implement above using something similar to the following
-- isComparable ti = isJust . (`findInfo` ti) . typ

findInfo :: (Instance -> Maybe a) -> Instances -> Maybe a
findInfo may = listToMaybe . mapMaybe may

findInfoOr :: a -> (Instance -> Maybe a) -> Instances -> a
findInfoOr def may = fromMaybe def . findInfo may

names :: Instances -> TypeRep -> [String]
names ti t = findInfoOr defNames m ti
  where
  m (Instance "Names" t' [ns]) | t == t' = Just $ eval defNames ns
  m _                                    = Nothing

tiersE :: Instances -> TypeRep -> [[Expr]]
tiersE ti t = findInfoOr (error $ "could not find Listable " ++ show t) m ti
  where
  m (Instance "Listable" t' [ts]) | t == t' = Just $ eval (error $ "invalid Listable " ++ show t) ts
  m _                                       = Nothing

eqE :: Instances -> TypeRep -> Maybe Expr
eqE ti t = findInfo m ti
  where
  m (Instance "Eq" t' [eq]) | t == t' = Just eq
  m _                                 = Nothing

ltE :: Instances -> TypeRep -> Maybe Expr
ltE ti t = findInfo m ti
  where
  m (Instance "Ord" t' [_,lt]) | t == t' = Just lt
  m _                                    = Nothing

leE :: Instances -> TypeRep -> Maybe Expr
leE ti t = findInfo m ti
  where
  m (Instance "Ord" t' [le,_]) | t == t' = Just le
  m _                                    = Nothing

deriving instance Typeable Word2 -- for GHC <= 7.8

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