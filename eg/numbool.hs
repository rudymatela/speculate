import Test.Speculate
import Test.LeanCheck ((==>))

main :: IO ()
main = speculate args
  { atoms =
      [ showConstant (0::Int)
      , showConstant (1::Int)
      , constant "+"   ((+)  :: Int -> Int -> Int)
      , constant "abs" (abs  :: Int -> Int)
      ]
  , conditionAtoms =
      [ constant "<="  ((<=) :: Int -> Int -> Bool)
      , constant "<"   ((<)  :: Int -> Int -> Bool)
      , constant "=="  ((==) :: Int -> Int -> Bool)
      , showConstant False
      , showConstant True
      , constant "not" not
    --, constant "&&"  (&&)
    --, constant "||"  (||)
      ]
  }
