import Test.Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { atoms =
      [ s (0 :: Int)
--    , s (1 :: Int)
      , s ([] :: [Int])
      , (:)     -:>  int  -| ":"
      , (++)    -:> [int] -| "++"
      , length  -:> [int] -| "length"
--    , (+)     -:> int   -| "+"
      ]
  }
