import Regex

main :: IO ()
main = do
  print $ Lit 'a'
  print $ (Empty :: RE Char)
