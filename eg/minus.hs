import Test.Speculate

main :: IO ()
main = speculate args
  { constants =
      [ showConstant (0::Int)
      , showConstant (1::Int)
      , constant "id"     $ id     -:> int
    -- Add abs for 30 seconds runtime in "old" KBC, about 10 without.
    --, constant "abs"    $ abs    -:> int
      , constant "+"      $ (+)    -:> int
      , constant "negate" $ negate -:> int
      , constant "-"      $ (-)    -:> int
      ]
  }
