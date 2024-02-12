-- |
-- Module      : Test.Speculate.Expr.Instance
-- Copyright   : (c) 2016-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Typeclass instance information.
module Test.Speculate.Expr.Instance
  ( Instances

  -- * reifying instances
  , reifyInstances
  , reifyInstances1
  , reifyListable, mkListable

  -- * checking instances
  , isListable, isListableT

  -- * finding functions
  , lookupTiers
  , lookupTiersT
  , holeOfTy, maybeHoleOfTy

  -- * the preludeInstances definition
  , preludeInstances

  -- * module re-export
  , module Data.Express.Instances
  )
where

import Data.Express.Instances
import Test.Speculate.Expr.Core
import Test.Speculate.Utils
import Test.LeanCheck
import Test.LeanCheck.Utils
import Data.Maybe

type Instances = [Expr] -- TODO: remove?

reifyInstances1 :: (Typeable a, Listable a, Show a, Eq a, Ord a, Name a) => a -> Instances
reifyInstances1 a  =  concat [reifyListable a, reifyEqOrd a, reifyName a]

reifyInstances :: (Typeable a, Listable a, Show a, Eq a, Ord a, Name a) => a -> Instances
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

reifyListable :: (Typeable a, Show a, Listable a) => a -> Instances
reifyListable a  =  mkListable (tiers -: [[a]])

mkListable :: (Typeable a, Show a) => [[a]] -> [Expr]
mkListable xss
  | all null xss  =  err
  | otherwise     =  [value "tiers" $ mapT val xss]
  where
  err  =  error
       $  "Speculate does not allow an empty tiers enumeration"
       ++ ", offending type: " ++ show (typeOf . head $ head xss)
-- TODO: reify an "undefined" value of a type to be able to holeOfTy and lift
--       the above restriction of no empty tiers?

isListable :: Instances -> Expr -> Bool
isListable is  =  isListableT is . typ

isListableT :: Instances -> TypeRep -> Bool
isListableT is  =  not . null . lookupTiersT is

lookupTiers :: Instances -> Expr -> [[Expr]]
lookupTiers is  =  lookupTiersT is . typ

lookupTiersT :: Instances -> TypeRep -> [[Expr]]
lookupTiersT is t  =  fromMaybe [] $ maybeTiersE is t
  where
  maybeTiersE :: Instances -> TypeRep -> Maybe [[Expr]]
  maybeTiersE is t  =  case i of
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
maybeHoleOfTy is t = case concat $ lookupTiersT is t of
                     (e:_) -> Just $ "" `varAsTypeOf` e
                     _     -> Nothing

-- despite the name, this _does not_ include most types from the prelude.
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
