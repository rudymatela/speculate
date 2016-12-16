import Test.Speculate

main :: IO ()
main = speculate args
  { showConditions = True
  , atoms =
      [ showConstant (0::Int)
      , showConstant (1::Int)
      , showConstant (2::Int)
      , showConstant False
      , showConstant True
      , constant "odd"    $ odd  -:> int
      , constant "even"   $ even -:> int
      , constant "`mod`"  $ mod  -:> int
      , constant "=="     $ (==) -:> int
      ]
  }
