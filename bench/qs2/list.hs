import QuickSpec

main = quickSpec signature
  { maxTermSize = Just 5
  , maxTermDepth = Just 4
  , maxTests = Just 500
  , constants =
      [ constant "[]" ([] :: [Int])
      , constant ":"  ((:) :: Int -> [Int] -> [Int])
      , constant "++" ((++) :: [Int] -> [Int] -> [Int])
      , constant "head" (head :: [Int] -> Int)
      , constant "tail" (tail :: [Int] -> [Int])
      ]
  }
