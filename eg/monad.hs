import Test.Speculate

import Control.Monad ((>=>))

import Test.LeanCheck
import Test.LeanCheck.Function
import Test.LeanCheck.Error (errorToNothing)
import Test.LeanCheck.Utils
import Data.Function (on)

type A = Int
type B = Int
type C = Int

funToList :: Listable a => (a -> b) -> [Maybe b]
funToList f = map (errorToNothing . f) list

instance (Listable a, Eq b) => Eq (a -> b) where
  (==) = (==) `on` (take 100 . funToList)

instance (Listable a, Ord b) => Ord (a -> b) where
  compare = compare `on` (take 100 . funToList)

main :: IO ()
main = speculate args
  { customTypeInfo = [typeInfo (int >- [int]) "f"]
  , atoms =
      [ hole (int >- [int])
      , constant "return" (return :: A -> [A])
      , constant ">>="    ((>>=) :: [A] -> (A -> [B]) -> [B])
      , constant ">=>"    ((>=>) :: (A -> [B]) -> (B -> [C]) -> (A -> [C]))
      ]
  }

-- TODO: make typeInfoF, which will not need typeclass instances
-- I actually should provide a selection of functions on each type!
-- So not only I enumerate listsOfPairs but also others
-- (maybe mutate my original seed functions for each type)
-- another option is to generate functions from holed expressions themselves!
