import Test.Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { constants =
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
      , constant "all"     $ all    ->:> [int]
      ]
  , backgroundConstants =
      [ constant "False"     False
      , constant "True"      True
      , constant "=="      $ (==)    -:>  int
      , constant "<="      $ (<=)    -:>  int
      ]
  , maxVars = 2
  , showConditions = True
  }
