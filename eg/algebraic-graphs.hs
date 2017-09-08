{-# LANGUAGE TemplateHaskell #-}
import Test.Speculate
import Test.Speculate.Expr.Instance (name)
import Data.Function (on)

import Algebra.Graph

deriveListable ''Graph

instance Ord a => Ord (Graph a) where
  (<=) = isSubgraphOf

main :: IO ()
main = speculate args
  { instances = [ins "x" (gr a), name "i" int]
  , constants =
      [ background
      , showConstant $ 0 -: a
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
  a :: Int
  a = undefined
  gr :: a -> Graph a
  gr _ = undefined
