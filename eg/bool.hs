import Speculate
import Test.LeanCheck ((==>))

main :: IO ()
main = speculate args
  { atoms =
      [ s False
      , s True
      , not   -| "not"
      , (&&)  -| "&&"
      , (||)  -| "||"
--    , (==>) -| "==>"
      ]
  }
