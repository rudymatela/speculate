import Test.Speculate

main :: IO ()
main = speculate args
  { instances = [ins "xy" (int,int)]
  , constants =
      [ background
      , constant "++"     $ (++)    -:> [int]
      , constant "==" $ (==) -:> int
      , constant "length" $ length  -:> [int]
      , foreground
      , constant "zip"    $ zip  -:> [int] ->:> [int]
      ]
  , showSemiequations = False
  , maxSemiSize = 5
  , maxCondSize = 5
  , maxVars = 3
  }
