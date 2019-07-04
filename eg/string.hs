import Test.Speculate

main :: IO ()
main = speculate args
  { instances = [ reifyInstances [string] ]
  , constants =
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
