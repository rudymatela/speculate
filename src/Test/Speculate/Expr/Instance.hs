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
  , TypeRep

  , ins

  -- * Queries on Instances
  , isListable, tiersE
  , maybeTiersE
  , holeOfTy
  , maybeHoleOfTy

  , listable
  , eq
  , ord
  , eqWith
  , ordWith

  -- * Type info for standard Haskell types
  , preludeInstances

  , boolTy
  , mkComparisonTy

  , module Data.Haexpress.Instances
  )
where

import Data.Haexpress.Instances
import Test.Speculate.Expr.Core hiding (eqWith)
import Test.Speculate.Expr.Match
import Test.Speculate.Utils hiding (ord)
import Test.LeanCheck
import Test.LeanCheck.Utils hiding (comparison)
import Data.Dynamic

import Data.Maybe (isJust,fromMaybe,listToMaybe,catMaybes,mapMaybe)
import Data.List (find,(\\))
import Data.Monoid ((<>))

type Instances = [Expr] -- TODO: remove?

-- TODO: export functions like the following?
--
-- reifyEq :: Eq a => a -> [Expr]
-- reifyOrd :: Ord a => a -> [Expr]
-- reifyEqOrd :: (Eq a, Ord a) => a -> [Expr]
-- reifyName :: ...
-- reifyListable :: Listable a => a -> [Expr]
-- reifyInstances1 :: (...) => a -> [Expr]
-- reifyInstances :: (...) => a -> [Expr]
--
-- the couple last ones replace ins1 and ins
--
-- Note that the return type is a list of Exprs for consistency.

-- | Usage: @ins1 "x" (undefined :: Type)@
ins1 :: (Typeable a, Listable a, Show a, Eq a, Ord a)
          => String -> a -> Instances
ins1 n x  =  concat [reifyEqOrd x, listable x, mkNameWith n x]

ins :: (Typeable a, Listable a, Show a, Eq a, Ord a)
    => String -> a -> Instances
ins n x = concat
  [    x      / n

  ,   [x]     / n ++ "s"
--,  [[x]]    / n ++ "ss"
--, [[[x]]]   / n ++ "ss"

  , (x,x)     / n ++ m
--, (x,x,x)   / n ++ m ++ o
--, (x,x,x,x) / n ++ m ++ o ++ p

--, [(x,x)]   / n ++ m ++ "s"
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
  m = variableNamesFromTemplate n !! 1
  o = variableNamesFromTemplate m !! 1
  p = variableNamesFromTemplate o !! 1
-- NOTE: the function typeInfoN is not perfect: it won't help produce types
-- combining different sub-types, like for example: (Bool,Int).  But it is
-- way better than the original version in which I had to explictly define
-- everything.  A definitive solution is still to be thought of.
-- NOTE: see related TODO on the definition of basicInstances

eq :: (Typeable a, Eq a) => a -> Instances
eq = reifyEq -- TODO: remove me

ord :: (Typeable a, Ord a) => a -> Instances
ord = reifyOrd -- TODO: remove me

listable :: (Typeable a, Show a, Listable a) => a -> Instances
listable = (:[]) . tiersFor

diffFor :: (Typeable a, Eq a) => a -> Expr
diffFor a  =  diffWith ((/=) -:> a)

diffWith :: Typeable a => (a -> a -> Bool) -> Expr
diffWith (/=)  =  value "/=" (/=)

lessEqFor :: (Typeable a, Ord a) => a -> Expr
lessEqFor a  =  lessEqWith ((<=) -:> a)

lessEqWith :: Typeable a => (a -> a -> Bool) -> Expr
lessEqWith (<=)  =  value "<=" (<=)

lessFor :: (Typeable a, Ord a) => a -> Expr
lessFor a  =  lessWith ((<) -:> a)

lessWith :: Typeable a => (a -> a -> Bool) -> Expr
lessWith (<)  =  value "<" (<)

eqWith :: (Typeable a, Eq a) => (a -> a -> Bool) -> Instances
eqWith  =  mkEq

ordWith :: (Typeable a, Ord a) => (a -> a -> Bool) -> Instances
ordWith  =  mkOrdLessEqual

tiersFor :: (Typeable a, Listable a, Show a) => a -> Expr
tiersFor a  =  tiersWith (tiers -: [[a]])

-- 'tiers' here are encoded indirectly because we need tiers of [[Expr]] when
-- computing 'grounds'.
tiersWith :: (Typeable a, Show a) => [[a]] -> Expr
tiersWith xss  =  value "tiers" $ mapT val xss

isListable :: Instances -> TypeRep -> Bool
isListable is = isJust . maybeTiersE is

maybeTiersE :: Instances -> TypeRep -> Maybe [[Expr]]
maybeTiersE is t = case i of
  [] -> Nothing
  (tiers:_) -> Just tiers
  where
  i = [tiers | e@(Value "tiers" _) <- is
             , let tiers = eval (undefined :: [[Expr]]) e
             , typ (head . concat $ tiers) == t]
-- TODO: on Reifying tiers enumerations, make explicit error showing that we
--       don't allow an empty tiers enumeration (hint: or use 'headOr' above)

holeOfTy :: Instances -> TypeRep -> Expr
holeOfTy is t = fromMaybe err $ maybeHoleOfTy is t
  where
  err  =  error $ "holeOfTy: could not find tiers with type `[[" ++ show t ++ "]]'."

maybeHoleOfTy :: Instances -> TypeRep -> Maybe Expr
maybeHoleOfTy is t = case concat <$> maybeTiersE is t of
                     Just (e:_) -> Just $ "" `varAsTypeOf` e
                     _          -> Nothing

tiersE :: Instances -> TypeRep -> [[Expr]]
tiersE is t = fromMaybe err $ maybeTiersE is t
  where
  err  =  error $ "Could not find tiers with type `[[" ++ show t ++ "]]'."

deriving instance Typeable Word2 -- for GHC <= 7.8

instance Name Word2

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
--, ins "x" (undefined :: Word2)
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
