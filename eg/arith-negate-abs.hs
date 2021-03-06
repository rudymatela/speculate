import Test.Speculate

main :: IO ()
main = speculate args
  { constants =
      [ showConstant (0::Int)
      , showConstant (1::Int)
      , constant "id"     (id     :: Int -> Int)
      , constant "abs"    (abs    :: Int -> Int)
      , constant "negate" (negate :: Int -> Int)
      , constant "+"      ((+)    :: Int -> Int -> Int)
      , constant "*"      ((*)    :: Int -> Int -> Int)
      ]
--, backgroundConstants =
--    [ constant "<="  ((<=) :: Int -> Int -> Bool)
--    , constant "<"   ((<)  :: Int -> Int -> Bool)
--    ]
  }
