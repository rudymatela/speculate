{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
import Test.Speculate
import Test.LeanCheck hiding ((\/))
import Test.LeanCheck.Utils
import Data.Function (on)
import Data.List (isSubsequenceOf)

import Digraph
import Data.Typeable (Typeable1) -- for GHC < 7.8

#if __GLASGOW_HASKELL__ < 708
deriving instance Typeable1 Digraph
#else
deriving instance Typeable Digraph
#endif
deriving instance Typeable Nat     -- for GHC < 7.10

instance Ord a => Ord (Digraph a) where
  g1 <= g2 = all (`elem` nodes g2) (nodes g1)
          && all (`elem` edges g2) (edges g1)

instance (Ord a, Listable a) => Listable (Digraph a) where
  tiers = concatMapT graphs $ setsOf tiers
    where
    graphs ns = mapT (D . zip ns)
              $ listsOfLength (length ns) (setsOf $ toTiers ns)

digraph :: a -> Digraph a
digraph = undefined

main :: IO ()
main = speculate args
  { instances =
      [ ins "x" nat
      , ins "a" (digraph nat)
      ]
  , maxTests = 500
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
      [ background

      , showConstant ([]::[Nat])
      , constant "elem"         $ elem    ->:> [nat]


      , foreground

      , constant "empty"    $ empty    -: digraph nat
      , constant "addNode"  $ addNode' -:>  nat
      , constant "addEdge"  $ addEdge' -:>  nat

      , constant "isNode"   $ isNode   -:>  nat
      , constant "isEdge"   $ isEdge   -:>  nat

      , constant "isPath"   $ isPath   -:>  nat
      , constant "subgraph" $ subgraph -:> [nat]
      ]
  }
