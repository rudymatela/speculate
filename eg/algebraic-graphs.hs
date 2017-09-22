{-# LANGUAGE TemplateHaskell #-}
import Test.Speculate
import Test.Speculate.Expr.Instance (name)
import Data.Function (on)
import Control.Monad (unless)

import Algebra.Graph

instance Ord a => Ord (Graph a) where
  (<=) = isSubgraphOf

-- deriveListable ''Graph {-
instance (Ord a, Listable a) => Listable (Graph a) where
  tiers = concatMapT graphs $ setsOf tiers
    where
    graphs ns = mapT (fromAdjList . zip ns)
              $ listsOfLength (length ns) (setsOf $ toTiers ns)
    fromAdjList :: [(a,[a])] -> Graph a
    fromAdjList ness = graph [n | (n,_) <- ness]
                             [(n1,n2) | (n1,n2s) <- ness, n2 <- n2s]
-- -}

main :: IO ()
main = do
  unless (listableGraphOK 180 a) $
    error "incorrect Listable (Graph a), see source"
  speculate args
    { instances = [ins "x" (gr a), ins "i" a]
    , maxTests = 1080
    , constants =
        [ background
        , showConstant $ 0 -: a
        , showConstant $ 0 -: int
        , showConstant True

        , foreground
        , constant "empty"     (empty   -:  gr a)
        , constant "vertex"    (vertex  -:> a)
        , constant "+"         ((+) -:> gr a)
        , constant "*"         ((*) -:> gr a)
        , constant "overlay"   (overlay -:> gr a)
        , constant "connect"   (connect -:> gr a)
        , constant "edge"      (edge    -:> a)
        , constant "length"    (length  -:> gr a)
        , constant "size"      (size    -:> gr a)
        ]
    , showConditions = False
    , maxSemiSize = 4
    , maxCondSize = 4
    }
  where
  a :: Nat3
  a = undefined

gr :: a -> Graph a
gr _ = undefined


-- tests for the Listable (Graph a) implementation:
listableGraphOK :: (Listable a, Ord a) => Int -> a -> Bool
listableGraphOK n x = and
  [ take n list `subset` take m (listGraphsInnefficient -: [gr x]) -- sound
  , take n (listGraphsInnefficient -: [gr x]) `subset` take m list -- complete
  ]
  where
  m = 60*n

-- innefficient reference implementation:
listGraphsInnefficient :: (Ord a, Listable a) => [Graph a]
listGraphsInnefficient = concat tiersGraphsInnefficient

tiersGraphsInnefficient :: (Ord a, Listable a) => [[Graph a]]
tiersGraphsInnefficient = cons0 empty
                       \/ cons1 vertex
                       \/ cons2 overlay
                       \/ cons2 connect

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs
