import QuickSpec
import Data.List (isSubsequenceOf)

main = quickSpec signature
  { maxTermSize = Just 7
  , maxTests = Just 500
  , constants =
      [ constant "[]" ([] :: [Int])
      , constant ":"  ((:) :: Int -> [Int] -> [Int])
      , constant "++" ((++) :: [Int] -> [Int] -> [Int])
--    , constant "head" (head :: [Int] -> Int)
--    , constant "tail" (tail :: [Int] -> [Int])
      , constant "0" (0 :: Int)
      , constant "length" (length :: [Int] -> Int)
--    , constant "<=" (isSubsequenceOf :: [Int] -> [Int] -> Bool)
      , constant "<=" ((<=) :: Int -> Int -> Bool)
      , constant "True" True
      ]
  }
