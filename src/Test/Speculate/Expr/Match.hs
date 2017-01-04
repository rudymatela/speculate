module Test.Speculate.Expr.Match
  ( Binds
  -- * Assigning
  , fill
  , assign
  , assigning
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

type Binds = [(String,Expr)]

findB :: String -> TypeRep -> Binds -> Maybe Expr
findB n t bs = snd <$> find (\(n',e) -> n' == n && typ e == t) bs

updateAssignments :: String -> Expr -> Binds -> Maybe Binds
updateAssignments s e = \bs ->
  case findB s (typ e) bs of
    Nothing -> Just ((s,e):bs)
    Just e' -> if e' == e
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
  fill' (Var "" t) (e:es) | t == typ e = (e,es)
  fill' e es = (e,es)

-- | Assign all occurences of a variable in an expression.
--
-- Examples in pseudo-Haskell:
--
-- > assign "x" (10) (x + y) = (10 + y)
-- > assign "y" (y + z) ((x + y) + (y + z)) = (x + (y + z)) + ((y + z) + z)
--
-- This respects the type (won't change occurrences of a similarly named
-- variable of a different type).
assign :: String -> Expr -> Expr -> Expr
assign n e (e1 :$ e2) = assign n e e1 :$ assign n e e2
assign n e (Var n' t) | t == typ e && n == n' = e
assign n e e1 = e1

-- | Assign all occurrences of several variables in an expression.
--
-- For single variables, this works as assign:
--
-- > x + y `assigning` [("x",10)] = (10 + y)
-- > ((x + y) + (y + z)) `assigning` [("y",y+z)] = (x + (y + z)) + ((y + z) + z)
--
-- Note this is /not/ equivalent to @foldr (uncurry assign)@.  Variables inside
-- expressions being assigned will not be assigned.
assigning :: Expr -> Binds -> Expr
(e1 :$ e2) `assigning` as = (e1 `assigning` as) :$ (e2 `assigning` as)
(Var n t) `assigning` as = fromMaybe (Var n t) $ findB n t as
e `assigning` _ = e

-- | Substitute matching subexpressios.
--
-- sub (x + y) 0 ((x + y) + z) == (0 + z)
-- sub (x + y) 0 (x + (y + z)) == (x + (y + z))
sub :: Expr -> Expr -> Expr -> Expr
sub ef et = s
  where
  s e | e == ef = et
  s (e1 :$ e2)  = s e1 :$ s e2
  s e           = e

-- | Primeify variable names in an expression.
--
-- > renameBy (++ "'") (x + y) = (x' + y')
-- > renameBy (++ "'") (y + (z + x)) = (y' + (z' + x'))
-- > renameBy (++ "1") abs x = abs x1
-- > renameBy (++ "2") abs (x + y) = abs (x2 + y2)
--
-- Note this will affect holes!
renameBy :: (String -> String) -> Expr -> Expr
renameBy f (e1 :$ e2) = (renameBy f e1) :$ (renameBy f e2)
renameBy f (Var n t) = Var (f n) t
renameBy f e = e

-- | List matches if possible
--
-- > 0 + 1       `match` x + y       = Just [x=0, y=1]
-- > 0 + (1 + 2) `match` x + y       = Just [x=0, y=1 + 2]
-- > 0 + (1 + 2) `match` x + (y + y) = Nothing
-- > (x + x) + (1 + 2) `match` x + (y + y) = Nothing
match :: Expr -> Expr -> Maybe Binds
match e1' e2' = matchWith [] e1' e2'

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
  m e1 e2 | typ e1 /= typ e2 = const Nothing
  m e1 (Var s t) = updateAssignments s e1
  m (f1 :$ x1) (f2 :$ x2) = m f1 f2 >=> m x1 x2
  m e1 e2 | e1 == e2  = Just
          | otherwise = const Nothing

unify :: Expr -> Expr -> Maybe Expr
unify e1 e2 = (e1' `assigning`) <$> unification e1' e2'
  where
  e1' = renameBy (++ "1") e1
  e2' = renameBy (++ "2") e2

-- NOTE: Take care of passing disjoing variable namespaces on both expressions!
-- see unify for an example of that.
unification :: Expr -> Expr -> Maybe Binds
unification e1' e2' = u e1' e2' []
  where
  u :: Expr -> Expr -> Binds -> Maybe Binds
  u e1 e2 | typ e1 /= typ e2 = const Nothing
  u e1@(Var s1 t1) e2@(Var s2 t2) = updateAssignments s1 e2 >=> updateAssignments s2 e1
  u e1 (Var s t) = updateAssignments s e1
  u (Var s t) e2 = updateAssignments s e2
  u (f1 :$ x1) (f2 :$ x2) = u f1 f2 >=> u x1 x2
  u e1 e2 | e1 == e2  = Just
          | otherwise = const Nothing

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
