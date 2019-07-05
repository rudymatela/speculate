import Test.Speculate

main :: IO ()
main = speculate args
  { constants =
      [ showConstant False
      , showConstant True
      , constant "not" not
      , constant "&&"  (&&)
      , constant "||"  (||)
--    , constant "==>" (==>)
      ]
  }
