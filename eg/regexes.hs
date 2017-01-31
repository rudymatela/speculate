import Regex

main :: IO ()
main = do
  print $ Lit 'a'
  print $ (Empty :: RE Char)
  print $ stringToSymbols "asdf" =~ Lit (Symbol 'a')
  print $ stringToSymbols "a" =~ Lit (Symbol 'a')
  print $ stringToSymbols "aa" =~ Lit (Symbol 'a')
  print $ stringToSymbols "aa" =~ Star (Lit (Symbol 'a'))
