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
  { customTypeInfo =
      [ typeInfo  nat "x"
      , typeInfo (digraph nat) "a"
      ]
  , maxTests = 6000
  , maxVars = 2
  , showConditions = True
  , constants =
    let
      -- totalize functions for nicer output
      -- (but also works without totalized functions)
      addNode' n g | isNode n g = g
                   | otherwise  = addNode n g
      addEdge' n1 n2 g | isEdge n1 n2 g = g
                       | otherwise = addEdge n1 n2 g
    in
      [ constant "emptyDigraph" $ emptyDigraph -: digraph nat
      , constant "preds"        $ preds    -:>  nat
      , constant "succs"        $ succs    -:>  nat
      , constant "isNode"       $ isNode   -:>  nat
      , constant "isEdge"       $ isEdge   -:>  nat
      , constant "isPath"       $ isPath   -:>  nat
      , constant "addNode"      $ addNode' -:>  nat
      , constant "addEdge"      $ addEdge' -:>  nat
      , constant "subgraph"     $ subgraph -:> [nat]
      ]
  , backgroundConstants =
      [ showConstant False
      , showConstant True
      , showConstant ([]::[Nat])
      , constant "elem"         $ elem    ->:> [nat]
      ]
  }
