import Test.Speculate
import Data.List (sort,insert)

ordered :: Ord a => [a] -> Bool
ordered (x:y:xs) = x <= y && ordered (y:xs)
ordered _        = True

main :: IO ()
main = speculate args
  { constants =
      [ constant "insert"  $ insert  -:>  int
      , constant "sort"    $ sort    -:> [int]
      ]
  , backgroundConstants =
      [ constant "False"     False
      , constant "True"      True
      , showConstant ([] :: [Int])
      , constant "=="      $ (==)    -:>  int
      , constant "<="      $ (<=)    -:>  int
      , constant "<"       $ (<)     -:>  int
      , constant ":"       $ (:)     -:>  int
      , constant "++"      $ (++)    -:> [int]
      , constant "elem"    $ elem   ->:> [int]
      , constant "ordered" $ ordered -:> [int]
      , constant "all"     $ all    ->:> [int]
      ]
  , maxVars = 2
  , showConditions = True
  }
