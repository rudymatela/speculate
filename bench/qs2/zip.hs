import QuickSpec hiding (insert)
import Data.List (sort, insert)
import Test.QuickCheck
import Data.Dynamic
import Control.Monad

main = quickSpec signature
  { maxTermSize = Just 7
  , maxTests = Just 500
  , constants =
      [ constant "++" ((++) :: [Int] -> [Int] -> [Int])
      , constant "length" (length :: [Int] -> Int)
      , constant "zip"   (zip :: [Int] -> [Int] -> [(Int,Int)])
      ]
  , predicates =
--    [ predicate "eqLen" ((\xs ys -> length xs == length ys) :: [Int] -> [Int] -> Bool)
      [ predicateGen "eqLen" ((\xs ys -> length xs == length ys) :: [Int] -> [Int] -> Bool) eqLenGen
      ]
  }

-- taken from quickspec/examples/Conditionals.hs
eqLenGen :: Gen [Dynamic]
eqLenGen = do
  len <- arbitrary
  xs <- (replicateM len arbitrary :: Gen [Int])
  ys <- (replicateM len arbitrary :: Gen [Int])
  return [toDyn xs, toDyn ys]
