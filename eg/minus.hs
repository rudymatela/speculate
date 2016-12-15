import Speculate

main :: IO ()
main = speculate args
  { atoms =
      [ s (0::Int)
      , s (1::Int)
      , id     -:> int -| "id"
    -- Add abs for 30 seconds runtime in "old" KBC, about 10 without.
    --, abs    -:> int -| "abs"
      , negate -:> int -| "negate"
      , (+)    -:> int -| "+"
      , (-)    -:> int -| "-"
      ]
  }
