-- |
-- Module      : Test.Speculate.Expr.Core
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- This module reexports 'Data.Express' along with some extra utilities.
module Test.Speculate.Expr.Core
  ( module Data.Express
  , module Data.Express.Utils.Typeable

  -- * Order
  , compareLexicographicallyBy
  , compareComplexityThenIndex

  -- * Properties
  , isConstantNamed
  , unrepeatedVars
  , isAssignment

  -- * Assigning
  , Binds

  -- * Matching
  , unify
  , unification
  , unificationC
  , isCanonInstanceOf
  , hasCanonInstanceOf

  -- * Commuting
  , commutations
  )
where

import Data.Express
import Data.Express.Utils.Typeable
import Test.Speculate.Utils
import Data.Monoid ((<>))
import Data.Functor ((<$>)) -- for GHC <= 7.8
import Data.Maybe (mapMaybe, listToMaybe)

-- | Lexicographical comparison of 'Expr's
--   where variables < constants < applications and
--   constants are disambiguated by the given function.
compareLexicographicallyBy :: (Expr -> Expr -> Ordering) -> Expr -> Expr -> Ordering
compareLexicographicallyBy compareConstants  =  cmp
  where
  (f :$ x) `cmp` (g :$ y)  =  f  `cmp` g <> x `cmp` y
  (_ :$ _) `cmp` _         =  GT
  _        `cmp` (_ :$ _)  =  LT
  e1 `cmp` e2  =  case (isVar e1, isVar e2) of
    (True,  True)  -> let Value n1 _ = e1
                          Value n2 _ = e2
                      in typ e1 `compareTy` typ e2
                      <> length n1 `compare` length n2
                      <> n1 `compare` n2
    (False, True)  -> GT
    (True,  False) -> LT
    (False, False) -> e1 `compareConstants` e2

-- | Comparison of 'Expr's:
--
-- 1. 'compareComplexity' from 'Data.Express'
-- 2. 'lexicompareBy' index of the given list
compareComplexityThenIndex :: [Expr] -> Expr -> Expr -> Ordering
compareComplexityThenIndex as  =  compareComplexity <> compareLexicographicallyBy cmp
  where
  e1 `cmp` e2 | arity e1 == 0 && arity e2 /= 0 = LT
  e1 `cmp` e2 | arity e1 /= 0 && arity e2 == 0 = GT
  e1 `cmp` e2 = compareIndex as e1 e2 <> e1 `compare` e2

countVars :: Expr -> [(Expr,Int)]
countVars e = map (\e' -> (e',length . filter (== e') $ vars e)) $ nubVars e

unrepeatedVars :: Expr -> Bool
unrepeatedVars = all (\(_,n) -> n == 1) . countVars

-- Is this espression an assignment of a variable to a value?
isAssignment :: Expr -> Bool
isAssignment ((Value "==" _ :$ e1) :$ e2) = isVar e1 || isVar e2
isAssignment _ = False

isConstantNamed :: Expr -> String -> Bool
e@(Value n' _) `isConstantNamed` n = isConst e && n' == n
_ `isConstantNamed` _ = False

type Binds = [(Expr,Expr)]

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
      if ex `isSubexprOf` e
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

unificationC :: [Expr] -> Expr -> Expr -> Maybe [(Expr,Expr)]
unificationC cos e  =  listToMaybe
                    .  mapMaybe (e `unification`)
                    .  commutations cos

commutations :: [Expr] -> Expr -> [Expr]
commutations cos  =  cmms
  where
  cmms (eo :$ ex :$ ey)  |  isValue eo && eo `elem` cos
                        =  concat [ [eo :$ ex' :$ ey', eo :$ ey' :$ ex']
                                  | ex' <- cmms ex
                                  , ey' <- cmms ey
                                  ]
  cmms (ef :$ ex)  =  [ ef' :$ ex'
                     | ef' <- cmms ef
                     , ex' <- cmms ex
                     ]
  cmms e  =  [e]
