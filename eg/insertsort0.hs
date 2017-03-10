import Test.Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { constants =
      [ background
      , showConstant ([] :: [Int])
      , constant ":"       $ (:)     -:>  int
      , foreground
      , constant "insert"  $ insert  -:>  int
      , constant "sort"    $ sort    -:> [int]

      , background
      , constant "<="      $ (<=)    -:>  int
      , constant "<"       $ (<)     -:>  int
      ]
  , maxVars = 2
  , showSemiequations = False
  , showConstantLaws = True
  }
