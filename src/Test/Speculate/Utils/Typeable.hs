-- |
-- Module      : Test.Speculate.Utils.Typeable
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Utilities to manipulate 'TypeRep's (of 'Typeable' values).
module Test.Speculate.Utils.Typeable
  ( module Data.Haexpress.Utils.Typeable
  , typesIn
  )
where

import Data.Haexpress.Utils.Typeable
import Data.Monoid ((<>))
import Test.Speculate.Utils.List ((+++))

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
