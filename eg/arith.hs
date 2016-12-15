import Speculate

main :: IO ()
main = speculate args
  { atoms =
      [ showConstant (0::Int)
      , showConstant (1::Int)
      , constant "id"     $ id     -:> int
      , constant "abs"    $ abs    -:> int
      , constant "negate" $ negate -:> int
      , constant "+"      $ (+)    -:> int
      , constant "*"      $ (*)    -:> int
      ]
  }
