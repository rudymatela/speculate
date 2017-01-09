import Test.Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { constants =
      [ showConstant ([] :: [Int])
      , constant ":"    $ (:)  -:>  int
      , constant "++"   $ (++) -:> [int]
      , constant "head" $ head -:> [int]
      , constant "tail" $ tail -:> [int]
--    , constant "null" $ null -:> [int]
      ]
  }
