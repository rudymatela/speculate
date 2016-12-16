module Test.Speculate.Utils.Digraph
  ( Digraph
  , empty
  , succs
  , preds
  , filter
  , discard
  , isNode
  , isEdge
  , fromEdges
  , narrow
  )
where

import Prelude hiding (filter)
import qualified Data.List as L
import Data.Maybe (fromMaybe,isJust)
import Test.Speculate.Utils (collectSndByFst)

type Digraph a = [(a,[a])]

empty :: Digraph a
empty = []

succs :: Eq a => a -> Digraph a -> [a]
succs x = fromMaybe [] . lookup x

preds :: Eq a => a -> Digraph a -> [a]
preds x yyss = [y | (y,ys) <- yyss, x `elem` ys]

isNode :: Eq a => a -> Digraph a -> Bool
isNode x = isJust . lookup x

isEdge :: Eq a => a -> a -> Digraph a -> Bool
isEdge x y d = y `elem` succs x d

filter :: Eq a => (a -> Bool) -> Digraph a -> Digraph a
filter p xxss = [(x,L.filter p xs) | (x,xs) <- xxss, p x]

discard :: Eq a => (a -> Bool) -> Digraph a -> Digraph a
discard p = filter (not . p)

subgraph :: Eq a => [a] -> Digraph a -> Digraph a
subgraph xs = filter (`elem` xs)

invsubgraph :: Eq a => [a] -> Digraph a -> Digraph a
invsubgraph xs = discard (`elem` xs)

fromEdges :: Ord a => [(a,a)] -> Digraph a
fromEdges = collectSndByFst

-- | pick a node in a Digraph
pick :: Eq a => Digraph a -> Maybe a
pick []            = Nothing
pick ((x,xs):xxss) = Just x

narrow :: Eq a => (a -> Bool) -> Digraph a -> [a]
narrow p d =
  case pick d of
    Nothing -> []
    Just n
      | p n -> case narrow p (subgraph (L.delete n $ succs n d) d) of
                 [] -> n:narrow p (invsubgraph (n:succs n d ++ preds n d) d)
                 xs -> xs
      | otherwise -> narrow p (invsubgraph (n:succs n d) d)
