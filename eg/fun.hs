import Test.Speculate
import Test.LeanCheck.Function
import Test.LeanCheck.Error (errorToNothing)
import Data.Function (on)

funToList :: Listable a => (a -> b) -> [Maybe b]
funToList f = map (errorToNothing . f) list

instance (Listable a, Eq b) => Eq (a -> b) where
  (==) = (==) `on` (take 100 . funToList)

instance (Listable a, Ord b) => Ord (a -> b) where
  compare = compare `on` (take 100 . funToList)

main :: IO ()
main = speculate args
  { maxTests = 1000
  , instances = [ins "f" (undefined :: Int -> Int)]
  , constants =
      [ hole (undefined :: Int -> Int)
      , constant "map" (map :: (Int -> Int) -> [Int] -> [Int])
      , constant "id"  (id  :: Int -> Int)
      , constant "."   ((.) :: (Int -> Int) -> (Int -> Int) -> (Int -> Int))
      ]
  }
