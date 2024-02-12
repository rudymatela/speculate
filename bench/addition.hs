import Test.Speculate

main :: IO ()
main = speculate args
  { maxSize = 7
  , showTheory = True
  , showSemiequations = False
  , showConditions = False
  , constants =
      [ showConstant (0::Int)
      , constant "+"  ((+) :: Int -> Int -> Int)
      ]
  }
