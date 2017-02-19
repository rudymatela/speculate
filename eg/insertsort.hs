import Test.Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { constants =
      [ constant "insert"  $ insert  -:>  int
      , constant "sort"    $ sort    -:> [int]
      ]
  , backgroundConstants =
    let
      ordered (x:y:xs) = x <= y && ordered (y:xs)
      ordered _        = True
    in
      [ constant "False"     False
      , constant "True"      True
      , showConstant ([] :: [Int])
      , constant "=="      $ (==)    -:>  int
      , constant "<="      $ (<=)    -:>  int
      , constant ":"       $ (:)     -:>  int
      , constant "++"      $ (++)    -:> [int]
      , constant "elem"    $ elem   ->:> [int]
      , constant "ordered" $ ordered -:> [int]
      , constant "all"     $ all    ->:> [int]
      ]
  , maxVars = 2
  , showConditions = True
  }
