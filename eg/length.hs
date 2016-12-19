import Test.Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { atoms =
      [ showConstant (0 :: Int)
--    , showConstant (1 :: Int)
      , showConstant ([] :: [Int])
      , constant ":"      $ (:)     -:>  int
      , constant "++"     $ (++)    -:> [int]
      , constant "length" $ length  -:> [int]
--    , constant "+"      $ (+)     -:>  int
      ]
  }
