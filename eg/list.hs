import Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { atoms =
      [ s ([] :: [Int])
      , (:)     -:>  int  -| ":"
      , (++)    -:> [int] -| "++"
      , head    -:> [int] -| "head"
      , tail    -:> [int] -| "tail"
      ]
  }
