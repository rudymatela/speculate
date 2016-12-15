import Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { atoms =
    let
      ordered (x:y:xs) = x <= y && ordered (y:xs)
      ordered _        = True
    in
      [ s ([] :: [Int])
      , (:)     -:>  int  -| ":"
      , (++)    -:> [int] -| "++"
      , insert  -:>  int  -| "insert"
      , sort    -:> [int] -| "sort"
      , elem   ->:> [int] -| "elem"
      , ordered -:> [int] -| "ordered"
      , False -| "False"
      , True  -| "True"
      , (==) -:> int -| "=="
      , (<=) -:> int -| "<="
      , all ->:> [int] -| "all"
      ]
  }
