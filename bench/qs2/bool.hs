import QuickSpec

main = quickSpec signature
  { maxTermSize = Just 5
  , constants =
      [ constant "False" False
      , constant "True"  True
      , constant "not" not
      , constant "&&" (&&)
      , constant "||" (||)
      ]
  }
