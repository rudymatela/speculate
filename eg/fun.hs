import Test.Speculate
import Test.Speculate.Function.A100

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
