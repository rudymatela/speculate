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
  , module Data.Typeable
  )
where

import Data.Typeable
import Test.Speculate.Utils.List ((+++))

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
  | otherwise = error "unFunTy: not a function type"

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
