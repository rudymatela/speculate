import Speculate

main :: IO ()
main = speculate args
  { atoms =
      [ s (0::Int)
      , s (1::Int)
      , id     -:> int -| "id"
      , (+)    -:> int -| "+"
      , (*)    -:> int -| "*"
      ]
  }
