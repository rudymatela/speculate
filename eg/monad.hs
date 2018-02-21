import Test.Speculate
import Test.Speculate.Function

import Control.Monad ((>=>))

type A = Int
type B = Int
type C = Int

instance (Listable a, Eq b) => Eq (a -> b)    where (==)    = areEqualFor 100
instance (Listable a, Ord b) => Ord (a -> b)  where compare = compareFor  100

main :: IO ()
main = speculate args
  { instances = [ins "f" (int >- [int])]
  , constants =
      [ hole (int >- [int])
      , constant "return" (return :: A -> [A])
      , constant ">>="    ((>>=) :: [A] -> (A -> [B]) -> [B])
      , constant ">=>"    ((>=>) :: (A -> [B]) -> (B -> [C]) -> (A -> [C]))
      ]
  }
