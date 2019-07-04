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
  , renameBy

  -- * Matching
  , match
  , match2
  , matchWith
  , unify
  , unification
  , isInstanceOf
  , hasInstanceOf
  , isCanonInstanceOf
  , hasCanonInstanceOf
  )
where

import Test.Speculate.Expr.Core

import Data.Typeable
import Data.List (find)
import Data.Maybe (isJust,fromMaybe)
import Data.Functor ((<$>))
import Test.Speculate.Utils
import Control.Monad ((>=>))

type Binds = [(Expr,Expr)]

findB :: Expr -> Binds -> Maybe Expr
findB e bs = snd <$> find ((e ==) . fst) bs

updateAssignments :: (Expr,Expr) -> Binds -> Maybe Binds
updateAssignments (e,e') = \bs ->
  case findB e bs of
    Nothing  -> Just ((e,e'):bs)
    Just e'' -> if e'' == e'
                then Just bs
                else Nothing

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

-- | Primeify variable names in an expression.
--
-- > renameBy (++ "'") (x + y) = (x' + y')
-- > renameBy (++ "'") (y + (z + x)) = (y' + (z' + x'))
-- > renameBy (++ "1") abs x = abs x1
-- > renameBy (++ "2") abs (x + y) = abs (x2 + y2)
--
-- Note this will affect holes!
renameBy :: (String -> String) -> Expr -> Expr
renameBy f = mapValues f'
  where
  f' (Value ('_':n) t) = Value ('_':f n) t
  f' e = e

-- | List matches if possible
--
-- > 0 + 1       `match` x + y       = Just [x=0, y=1]
-- > 0 + (1 + 2) `match` x + y       = Just [x=0, y=1 + 2]
-- > 0 + (1 + 2) `match` x + (y + y) = Nothing
-- > (x + x) + (1 + 2) `match` x + (y + y) = Nothing
match :: Expr -> Expr -> Maybe Binds
match = matchWith []

-- | List matches of pairs of expressions if possible
--
-- > (0,1)   `match2` (x,y)   = Just [x=0, y=1]
-- > (0,1+2) `match2` (x,y+y) = Nothing
match2 :: (Expr,Expr) -> (Expr,Expr) -> Maybe Binds
match2 (e1,e2) (e3,e4) =
  case matchWith [] e1 e3 of
    Nothing -> Nothing
    Just bs -> matchWith bs e2 e4

-- | List matches with preexisting bindings:
--
-- > 0 + 1 `matchWith [(x,0)]` x + y = Just [x=0, y=1]
-- > 0 + 1 `matchWith [(x,1)]` x + y = Nothing
matchWith :: Binds -> Expr -> Expr -> Maybe Binds
matchWith bs e1' e2' = m e1' e2' bs
  where
  m :: Expr -> Expr -> Binds -> Maybe Binds
  m (f1 :$ x1) (f2 :$ x2)             =  m f1 f2 >=> m x1 x2
  m e1 e2
    | isVar e2 && mtyp e1 == mtyp e2  =  updateAssignments (e2,e1)
    | e1 == e2                        =  Just
    | otherwise                       =  const Nothing

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

-- 0 `isInstanceOf` x = True
-- y `isInstanceOf` x = True
-- x `isInstanceOf` 0 = False
-- 1 `isInstanceOf` 0 = False
-- x + (y + x) `isInstanceOf` x + y = True
-- y + (y + x) `isInstanceOf` x + y = True
-- 0 + (y + x) `isInstanceOf` x + y = True
-- x `isInstanceOf` x = True
-- _ `isInstanceOf` x = True
isInstanceOf :: Expr -> Expr -> Bool
e1 `isInstanceOf` e2 = isJust $ e1 `match` e2

hasInstanceOf :: Expr -> Expr -> Bool
e1           `hasInstanceOf` e2 | e1   `isInstanceOf` e2 = True
(e1f :$ e1x) `hasInstanceOf` e2 | e1f `hasInstanceOf` e2 ||
                                  e1x `hasInstanceOf` e2 = True
_            `hasInstanceOf` _                           = False

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
