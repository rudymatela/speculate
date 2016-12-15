import Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { atoms =
    let
      ordered (x:y:xs) = x <= y && ordered (y:xs)
      ordered _        = True
    in
      [ showConstant ([] :: [Int])
      , constant ":"       $ (:)     -:>  int
      , constant "++"      $ (++)    -:> [int]
      , constant "insert"  $ insert  -:>  int
      , constant "sort"    $ sort    -:> [int]
      , constant "elem"    $ elem   ->:> [int]
      , constant "ordered" $ ordered -:> [int]
      , constant "False"     False
      , constant "True"      True
      , constant "=="      $ (==)    -:>  int
      , constant "<="      $ (<=)    -:>  int
      , constant "all"     $ all    ->:> [int]
      ]
  }
