import Test.Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { atoms = 
      [ s ""
      , s " "
      , s "\n"

      , lines   -| "lines"
      , words   -| "words"
      , unlines -| "unlines"
      , unwords -| "unwords"

      , (++) -:> [char] -| "++"
      ]
  }
