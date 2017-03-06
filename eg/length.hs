import Test.Speculate
import Data.List (isSubsequenceOf)

main :: IO ()
main = speculate args
  { constants =
      [ background
      , showConstant ([] :: [Int])
      , constant ":"      $ (:)     -:>  int
      , constant "++"     $ (++)    -:> [int]
      , showConstant (0 :: Int)
--    , showConstant (1 :: Int)
      , foreground
      , constant "length" $ length  -:> [int]
      ]
  , instances = [ ordWith (isSubsequenceOf :: [Int] -> [Int] -> Bool) ]
  }
