import QuickSpec hiding (insert)
import Data.List (sort, insert)
import Test.QuickCheck
import Data.Dynamic

main = quickSpec signature
  { maxTermSize = Just 5 -- 9
  , constants =
      [ constant "[]" ([] :: [Int])
      , constant ":"  ((:) :: Int -> [Int] -> [Int])
      , constant "++" ((++) :: [Int] -> [Int] -> [Int])
      , constant "insert" (insert :: Int -> [Int] -> [Int])
      , constant "sort"   (sort :: [Int] -> [Int])
      ]
  , predicates =
      [ predicateGen "<=" ((<=) :: Int -> Int -> Bool) leGen
      , predicateGen "<"  ((<)  :: Int -> Int -> Bool) ltGen
--    [ predicate "-<=-" ((\x y xs -> x <= y) :: Int -> Int -> [Int] -> Bool)
      ]
  }

leGen :: Gen [Dynamic]
leGen = do
  x1 <- (arbitrary :: Gen Int)
  x2 <- (arbitrary :: Gen Int)
  return [toDyn $ x1, toDyn $ x1 + x2]

ltGen :: Gen [Dynamic]
ltGen = do
  x1 <- (arbitrary :: Gen Int)
  x2 <- (arbitrary :: Gen Int)
  return [toDyn $ x1, toDyn $ x1 + x2 + 1]
