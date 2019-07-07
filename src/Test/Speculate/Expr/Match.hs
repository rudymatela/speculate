-- |
-- Module      : Test.Speculate.Expr.Match
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Matching expressions.
module Test.Speculate.Expr.Match
  ( Binds
  -- * Assigning
  , fill
  , sub

  -- * Matching
  , unify
  , unification
  , isCanonInstanceOf
  , hasCanonInstanceOf
  )
where

import Test.Speculate.Expr.Core

import Data.List (lookup)
import Data.Maybe
import Data.Functor ((<$>))
import Test.Speculate.Utils
import Control.Monad ((>=>))

type Binds = [(Expr,Expr)]

-- | Fill holes in an expression.
--   Silently skips holes that are not of the right type.
--   Silently discard remaining expressions.
fill :: Expr -> [Expr] -> Expr
fill e = fst . fill' e
  where
  fill' :: Expr -> [Expr] -> (Expr,[Expr])
  fill' (e1 :$ e2) es = let (e1',es')  = fill' e1 es
                            (e2',es'') = fill' e2 es'
                        in (e1' :$ e2', es'')
  fill' eh (e:es) | isHole eh && typ eh == typ e = (e,es)
  fill' e es = (e,es)

-- | Substitute matching subexpressios.
--
-- sub (x + y) 0 ((x + y) + z) == (0 + z)
-- sub (x + y) 0 (x + (y + z)) == (x + (y + z))
--
-- TODO: remove
sub :: Expr -> Expr -> Expr -> Expr
sub ef et = (// [(ef,et)])

unify :: Expr -> Expr -> Maybe Expr
unify e1 e2 = (e1 //-) <$> unification e1 e2

unification :: Expr -> Expr -> Maybe [(Expr,Expr)]
unification = naiveUnification

findBind :: Expr -> Expr -> Either Bool (Expr,Expr)
findBind e1         e2          |  typ e1 /= typ e2  =  Left False
                                |  e1 == e2          =  Left True
                                |  isVar e1          =  Right (e1,e2)
                                |  isVar e2          =  Right (e2,e1)
findBind (f1 :$ x1) (f2 :$ x2)  =  case findBind f1 f2 of
                                   Left True -> findBind x1 x2
                                   r         -> r
findBind e1         e2          =  Left (e1 == e2)

-- NOTE: there are faster methods for unification.
naiveUnification :: Expr -> Expr -> Maybe [(Expr,Expr)]
naiveUnification e1' e2' = uu e1' e2' []
  where
  uu :: Expr -> Expr -> Binds -> Maybe Binds
  uu e1' e2' bs' =
    case u e1' e2' bs' of
      Nothing -> Nothing
      Just (e1,e2,bs) ->
        if e1' == e1 && e2' == e2
        then Just bs
        else uu e1 e2 bs
  u :: Expr -> Expr -> Binds -> Maybe (Expr,Expr,Binds)
  u e1 e2 bs =
    case findBind e1 e2 of
    Left False -> Nothing
    Left True  -> Just (e1,e2,bs)
    Right (ex,e) ->
      if ex `isSubexpr` e
      then Nothing
      else Just ( e1 //- [(ex,e)]
                , e2 //- [(ex,e)]
                , (ex,e):[(ex',e' //- [(ex,e)]) | (ex',e') <- bs]
                )

isCanonInstanceOf :: Expr -> Expr -> Bool
e1 `isCanonInstanceOf` e2 =
  case e1 `match` e2 of
    Nothing -> False
    Just xs -> strictlyOrderedOn snd (sortOn fst xs)

hasCanonInstanceOf :: Expr -> Expr -> Bool
e1           `hasCanonInstanceOf` e2 | e1   `isCanonInstanceOf` e2 = True
(e1f :$ e1x) `hasCanonInstanceOf` e2 | e1f `hasCanonInstanceOf` e2 ||
                                       e1x `hasCanonInstanceOf` e2 = True
_            `hasCanonInstanceOf` _                                = False
