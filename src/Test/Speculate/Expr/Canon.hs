-- |
-- Module      : Test.Speculate.Expr.Canon
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Canonicalize and check canonicity of expressions.
module Test.Speculate.Expr.Canon
  ( canonicalize
  , canonicalizeWith
  , canonicalizationWith
  , canonicalWith
  )
where

import Test.Speculate.Expr.Core
import Test.Speculate.Expr.Match
import Test.Speculate.Expr.Instance
import Data.List ((\\))

-- | Canonicalize variable names in an expression.
--
-- > canonicalize (x + y) = (x + y)
-- > canonicalize (y + x) = (x + y)
-- > canonicalize (y + (z + x)) = (x + (y + z))
-- > canonicalize ((w + z) + (z + x)) = ((x + y) + (y + z))
-- > canonicalize (y + abs y) = (x + abs x)
-- > canonicalize ((y + x) == (x + y)) = ((x + y) == (y + x))
canonicalizeWith :: Instances -> Expr -> Expr
canonicalizeWith is e = e //- canonicalizationWith is e

canonicalizationWith :: Instances -> Expr -> [(Expr,Expr)]
canonicalizationWith is e = cr (vars e) []
  where
  cr :: [Expr] -> [(Expr,Expr)] -> [(Expr,Expr)]
  cr []     bs  =  bs
  cr (e:es) bs  =  cr es
                $ if e `elem` map fst bs
                  then bs
                  else (e, (`varAsTypeOf` e) . head $ names is e \\ [n | (_,Value ('_':n) _) <- bs]):bs

canonicalize :: Expr -> Expr
canonicalize = canonicalizeWith preludeInstances

canonicalWith :: Instances -> Expr -> Bool
canonicalWith ti e = canonicalizeWith ti e == e
