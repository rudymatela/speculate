-- |
-- Module      : Test.Speculate.Utils.Typeable
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Utilities to manipulate 'TypeRep's (of 'Typeable' values).
module Test.Speculate.Utils.Typeable
  ( tyArity
  , typesIn
  , unFunTy
  , isFunTy
  , argumentTy
  , resultTy
  , finalResultTy
  , boolTy
  , mkEqnTy
  , funTyCon
  , compareTy
  , module Data.Typeable
  )
where

import Data.Typeable
import Data.Monoid ((<>))
import Test.Speculate.Utils.List ((+++))

-- Different versions of Typeable/GHC provide different orderings for TypeReps.
-- The following is a version independent ordering, with the following
-- properties:
--
-- * functional types with more arguments are larger;
-- * type constructors with more arguments are larger.
compareTy :: TypeRep -> TypeRep -> Ordering
compareTy t1 t2 | t1 == t2 = EQ -- optional optimization
compareTy t1 t2 = tyArity t1 `compare` tyArity t2
               <> length ts1 `compare` length ts2
               <> show c1 `compare` show c2
               <> foldr (<>) EQ (zipWith compareTy ts1 ts2)
  where
  (c1,ts1) = splitTyConApp t1
  (c2,ts2) = splitTyConApp t2

tyArity :: TypeRep -> Int
tyArity t
  | isFunTy t = 1 + tyArity (resultTy t)
  | otherwise = 0

-- | For a given type, return all *-kinded types.
--   (all non-function types)
--
-- > typesIn (typeOf (undefined :: (Int -> Int) -> Int -> Bool))
-- >   == [Bool,Int]
typesIn :: TypeRep -> [TypeRep]
typesIn t
  | isFunTy t = typesIn (argumentTy t)
            +++ typesIn (resultTy   t)
  | otherwise = [t]

finalResultTy :: TypeRep -> TypeRep
finalResultTy t
  | isFunTy t = finalResultTy (resultTy t)
  | otherwise = t

unFunTy :: TypeRep -> (TypeRep,TypeRep)
unFunTy t
  | isFunTy t = let (f,[a,b]) = splitTyConApp t in (a,b)
  | otherwise = error $ "error (unFunTy): `" ++ show t ++ "` is not a function type"

argumentTy :: TypeRep -> TypeRep
argumentTy = fst . unFunTy

resultTy :: TypeRep -> TypeRep
resultTy = snd . unFunTy

boolTy :: TypeRep
boolTy = typeOf (undefined :: Bool)

funTyCon :: TyCon
funTyCon = typeRepTyCon $ typeOf (undefined :: () -> ())

isFunTy :: TypeRep -> Bool
isFunTy t =
  case splitTyConApp t of
    (con,[_,_]) | con == funTyCon -> True
    _ -> False

mkEqnTy :: TypeRep -> TypeRep
mkEqnTy a = a `mkFunTy` (a `mkFunTy` boolTy)
