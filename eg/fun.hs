import Test.Speculate
import Test.Speculate.Function

instance (Listable a, Eq b) => Eq (a -> b)    where (==)    = areEqualFor 100
instance (Listable a, Ord b) => Ord (a -> b)  where compare = compareFor  100

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
