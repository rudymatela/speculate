import Test.Speculate
import Data.List (isSubsequenceOf)

main :: IO ()
main = speculate args
  { constants =
      [ constant "length" $ length  -:> [int]
      ]
  , backgroundConstants =
      [ showConstant ([] :: [Int])
      , constant ":"      $ (:)     -:>  int
      , constant "++"     $ (++)    -:> [int]
      , showConstant (0 :: Int)
--    , showConstant (1 :: Int)
      ]
  , instances = [ ordWith (isSubsequenceOf :: [Int] -> [Int] -> Bool) ]
  }
