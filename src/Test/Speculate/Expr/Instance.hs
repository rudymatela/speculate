{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC <= 7.8
-- |
-- Module      : Test.Speculate.Expr.Instance
-- Copyright   : (c) 2016-2019 Rudy Matela
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

  , reifyInstances
  , reifyInstances1
  , reifyListable, mkListable

  -- * Type info for standard Haskell types
  , preludeInstances

  , boolTy
  , mkComparisonTy

  , module Data.Haexpress.Instances
  )
where

import Data.Haexpress.Instances
import Test.Speculate.Expr.Core
import Test.Speculate.Utils hiding (ord)
import Test.LeanCheck
import Test.LeanCheck.Utils hiding (comparison)

import Data.Maybe
import Data.Monoid ((<>)) -- for GHC <= 8.2

type Instances = [Expr] -- TODO: remove?

-- TODO: make all functions follow the standard:
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

reifyInstances1 :: (Typeable a, Listable a, Show a, Eq a, Ord a, Name a)
                => a -> Instances
reifyInstances1 a  =  concat [reifyListable a, reifyEqOrd a, reifyName a]

reifyInstances :: (Typeable a, Listable a, Show a, Eq a, Ord a, Name a)
               => a -> Instances
reifyInstances a  =  concat
  [ r1 a
  , r1 [a]
--, r1 [[a]]
  , r1 (a,a)
--, r1 (a,a,a)
--, r1 [(a,a)]
  , r1 (mayb a)
--, r1 (eith a a)
  ]
  where
  r1 :: (Typeable a, Listable a, Show a, Eq a, Ord a, Name a)
     => a -> Instances
  r1 = reifyInstances1

-- | Usage: @ins1 "x" (undefined :: Type)@
ins1 :: (Typeable a, Listable a, Show a, Eq a, Ord a)
          => String -> a -> Instances
ins1 n x  =  concat [reifyEqOrd x, reifyListable x, mkNameWith n x]

ins :: (Typeable a, Listable a, Show a, Eq a, Ord a)
    => String -> a -> Instances
ins n x = concat
  [    x      / n
  ,   [x]     / n ++ "s"
--,  [[x]]    / n ++ "ss"
  , (x,x)     / n ++ m
--, (x,x,x)   / n ++ m ++ o
--, [(x,x)]   / n ++ m ++ "s"
  , mayb x    / "m" ++ n ++ "1"
--, eith x x  / "e" ++ n ++ o ++ "1"
  ]
  where
  (/) :: (Typeable a, Listable a, Show a, Eq a, Ord a)
      => a -> String -> Instances -- monomorphism restriction strikes again
  (/) = flip ins1
  infixr 0 /
  m = variableNamesFromTemplate n !! 1
-- NOTE: the function typeInfoN is not perfect: it won't help produce types
-- combining different sub-types, like for example: (Bool,Int).  But it is
-- way better than the original version in which I had to explictly define
-- everything.  A definitive solution is still to be thought of.
-- NOTE: see related TODO on the definition of basicInstances

reifyListable :: (Typeable a, Show a, Listable a) => a -> Instances
reifyListable a  =  mkListable (tiers -: [[a]])

mkListable :: (Typeable a, Show a) => [[a]] -> [Expr]
mkListable xss
  | null (concat xss)  =  err
  | otherwise          =  [value "tiers" $ mapT val xss]
  where
  err  =  error
       $  "Speculate does not allow an empty tiers enumeration"
       ++ ", offending type: " ++ show (typeOf . head $ head xss)
-- TODO: reify an "undefined" value of a type to be able to holeOfTy and lift
--       the above restriction of no empty tiers?

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
-- TODO: make the above work on empty tiers

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

preludeInstances :: Instances
preludeInstances  =  concat
  [ r1 (u :: ())
  , r1 (u :: [()])

  , r (u :: Bool)

  , r (u :: Int)
--, r (u :: Word)
  , r (u :: Integer)

  , r (u :: Ordering)
  , r (u :: Char)

  , r (u :: Rational)
  , r (u :: Float)
  , r (u :: Double)

-- TODO: uncomment the following and investigate why compilation takes so long
--, r (u :: Int1)
--, r (u :: Int2)
--, r (u :: Int3)
--, r (u :: Int4)
--, r (u :: Word1)
--, r (u :: Word2)
--, r (u :: Word3)
--, r (u :: Word4)
--, r (u :: Nat1)
--, r (u :: Nat2)
--, r (u :: Nat3)
--, r (u :: Nat4)
--, r (u :: Nat5)
--, r (u :: Nat6)
--, r (u :: Nat7)
  ]
  where
  u :: a
  u  =  undefined
  r, r1 :: (Typeable a, Listable a, Show a, Eq a, Ord a, Name a)
        => a -> Instances
  r = reifyInstances
  r1 = reifyInstances1
-- WHOA!  Have I discovered a "bug" in GHC?  adding to many type compositions
-- on ins and types on preludeInstances makes compilation of this module
-- *really* slow: it takes a whopping 2 minutes!
-- (the above report is using -O2, I have not tested without optimizations).
