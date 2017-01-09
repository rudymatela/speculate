import Test.Speculate
import Test.LeanCheck ((==>))

main :: IO ()
main = speculate args
  { constants =
      [ constant "+"   ((+)  :: Int -> Int -> Int)
      , constant "abs" (abs  :: Int -> Int)
      ]
  , backgroundConstants =
      [ showConstant (0::Int)
      , showConstant (1::Int)
      ]
  , conditionConstants =
      [ constant "<="  ((<=) :: Int -> Int -> Bool)
      , constant "<"   ((<)  :: Int -> Int -> Bool)
      , constant "=="  ((==) :: Int -> Int -> Bool)
      , showConstant False
      , showConstant True
      , constant "not" not
    --, constant "&&"  (&&)
    --, constant "||"  (||)
      ]
  , showConditions = True
  , maxVars        = 2
  , maxCondSize    = 0  -- == maxSize
  }
