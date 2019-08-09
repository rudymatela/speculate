import Test.Speculate
import Test.Speculate.Function.A100()

main :: IO ()
main = speculate args
  { maxTests = 1000
  , instances = [reifyInstances (undefined :: Int -> Int)]
  , constants =
      [ constant "map" (map :: (Int -> Int) -> [Int] -> [Int])
      , constant "id"  (id  :: Int -> Int)
      , constant "."   ((.) :: (Int -> Int) -> (Int -> Int) -> (Int -> Int))
      ]
  }
