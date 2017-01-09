import Test.Speculate
import Data.List (sort,insert)

main :: IO ()
main = speculate args
  { constants = 
      [ showConstant ""
      , showConstant " "
      , showConstant "\n"

      , constant "lines"   lines
      , constant "words"   words
      , constant "unlines" unlines
      , constant "unwords" unwords

      , constant "++" $ (++) -:> [char]
      ]
  }
