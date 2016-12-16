import Test.Speculate

main :: IO ()
main = speculate args
  { atoms =
      [ showConstant (0::Int)
      , showConstant (1::Int)
      , constant "id" (id :: Int -> Int)
      , constant "+"  ((+) :: Int -> Int -> Int)
      , constant "*"  ((*) :: Int -> Int -> Int)
      ]
  }
