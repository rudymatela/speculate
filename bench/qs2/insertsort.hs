import QuickSpec hiding (insert)
import Data.List (sort, insert)

main =
  quickSpec
    signature
      { maxTermSize = Just 5
      , constants =
          [ constant "[]" ([] :: [Int])
          , constant ":"  ((:) :: Int -> [Int] -> [Int])
          , constant "++" ((++) :: [Int] -> [Int] -> [Int])
          , constant "insert" (insert :: Int -> [Int] -> [Int])
          , constant "sort"   (sort :: [Int] -> [Int])
          ]
      }
