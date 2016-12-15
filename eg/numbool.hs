import Speculate
import Test.LeanCheck ((==>))

main :: IO ()
main = speculate args
  { atoms =
      [ s False
      , s True
      , not   -| "not"
    --, (&&)  -| "&&"
    --, (||)  -| "||"

      , s (0::Int)
      , s (1::Int)
      , (+)  -:> int -| "+"
      , (<=) -:> int -| "<="
      , (<)  -:> int -| "<"
      , (==) -:> int -| "=="
      , abs  -:> int -| "abs"
      ]
  }
