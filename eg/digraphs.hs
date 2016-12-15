import Speculate
import Test.LeanCheck hiding ((\/))
import Test.LeanCheck.Utils
import Data.Function (on)

import Digraph

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
