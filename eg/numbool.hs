import Speculate
import Test.LeanCheck ((==>))

main :: IO ()
main = speculate args
  { atoms =
      [ showConstant False
      , showConstant True
      , constant "not" not
    --, constant "&&"  (&&)
    --, constant "||"  (||)

      , showConstant (0::Int)
      , showConstant (1::Int)
      , constant "+"   ((+)  :: Int -> Int -> Int)
      , constant "<="  ((<=) :: Int -> Int -> Bool)
      , constant "<"   ((<)  :: Int -> Int -> Bool)
      , constant "=="  ((==) :: Int -> Int -> Bool)
      , constant "abs" (abs  :: Int -> Int)
      ]
  }
