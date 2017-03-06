import QuickSpec hiding (insert)
import Data.List (sort, insert)
import Test.QuickCheck
import Data.Dynamic

main :: IO ()
main =
  quickSpec signature
    { maxTermSize = Just 5
    , maxTests = Just 500
    , constants =
        [ constant "++" ((++) :: [Int] -> [Int] -> [Int])
        , constant "length" (length :: [Int] -> Int)
        , constant "zip"   (zip :: [Int] -> [Int] -> [(Int,Int)])
        ]
    , predicates =
        [ predicate "eqLen" ((\xs ys -> length xs == length ys) :: [Int] -> [Int] -> Bool)
        ]
    }
