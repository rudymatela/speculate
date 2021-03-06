import Test.Speculate hiding (A, B, C)
import Test.Speculate.Function.A100()

import Control.Monad ((>=>))

type A = Int
type B = Int
type C = Int

main :: IO ()
main = speculate args
  { instances = [reifyInstances (int >- [int])]
  , constants =
      [ constant "return" (return :: A -> [A])
      , constant ">>="    ((>>=) :: [A] -> (A -> [B]) -> [B])
      , constant ">=>"    ((>=>) :: (A -> [B]) -> (B -> [C]) -> (A -> [C]))
      ]
  }
