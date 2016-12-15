import QuickSpec

main =
  quickSpec
    signature
      { maxTermSize = Just 5
      , constants =
          [ constant "[]" ([] :: [Int])
          , constant ":"  ((:) :: Int -> [Int] -> [Int])
          , constant "++" ((++) :: [Int] -> [Int] -> [Int])
--        , constant "head" (head :: [Int] -> Int)
--        , constant "tail" (tail :: [Int] -> [Int])
          ]
      }
