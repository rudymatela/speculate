import Test.Speculate

main :: IO ()
main = speculate args
  { constants =
      [ constant "+"   ((+)  :: Int -> Int -> Int)
      , constant "id"  (id   :: Int -> Int)
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
      ]
  , showConditions = True
  , showConstantLaws = False
  , maxVars        = 2
  , maxCondSize    = 0  -- == maxSize
  }
