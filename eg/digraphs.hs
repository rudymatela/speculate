{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
import Test.Speculate
import Test.LeanCheck hiding ((\/))
import Test.LeanCheck.Utils
import Data.Function (on)

import Digraph
import Data.Typeable (Typeable1) -- for GHC < 7.8

#if __GLASGOW_HASKELL__ < 708
deriving instance Typeable1 Digraph
#else
deriving instance Typeable Digraph
#endif
deriving instance Typeable Nat     -- for GHC < 7.10

instance Ord a => Ord (Digraph a) where
  compare = compare `on` nodeSuccs

instance (Ord a, Listable a) => Listable (Digraph a) where
  tiers = concatMapT graphs $ setsOf tiers
    where
    graphs ns = mapT (D . zip ns)
              $ listsOfLength (length ns) (setsOf $ toTiers ns)

digraph :: a -> Digraph a
digraph = undefined

main :: IO ()
main = speculate args
  { typeInfo_ = typeInfo  nat "x"
              : typeInfo [nat] "xs"
              : typeInfo (digraph nat) "a"
              : basicTypeInfo
  , maxTests = 6000
  , atoms =
    let
      addNode' n g | isNode n g = g
                   | otherwise  = addNode n g
      addEdge' n1 n2 g | isEdge n1 n2 g = g
                       | otherwise = addEdge n1 n2 g
    in
      [ False -| "False"
      , True  -| "True"
      , ([]::[Nat]) -| "[]"
      , emptyDigraph -: digraph nat -| "emptyDigraph"
      , preds  -:> nat -| "preds"
      , succs  -:> nat -| "succs"
      , isNode -:> nat -| "isNode"
      , isEdge -:> nat -| "isEdge"
      , isPath -:> nat -| "isPath"
      , addNode' -:> nat -| "addNode"
      , addEdge' -:> nat -| "addEdge"
      , subgraph -:> [nat] -| "subgraph"
      , (==) -:> nat -| "=="
      , elem ->:> [nat] -| "elem"
      ]
  }
