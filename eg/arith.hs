import Test.Speculate

main :: IO ()
main = speculate args
  { atoms =
      [ showConstant (0::Int)
      , showConstant (1::Int)
      , constant "id"     (id     :: Int -> Int)
      , constant "abs"    (abs    :: Int -> Int)
      , constant "negate" (negate :: Int -> Int)
      , constant "+"      ((+)    :: Int -> Int -> Int)
      , constant "*"      ((*)    :: Int -> Int -> Int)
      ]
  }
