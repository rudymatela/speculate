import Test.Speculate
import Data.List (isSubsequenceOf)

main :: IO ()
main = speculate args
  { constants =
      [ background
      , constant "++"     $ (++)    -:> [int]
      , constant "==" $ (==) -:> int
      , foreground
      , constant "length" $ length  -:> [int]
      , constant "zip"    $ zip  -:> [int] ->:> [int]
      ]
  , showSemiequations = False
  , maxSemiSize = 5
  , maxCondSize = 5
  , maxVars = 3
  }
