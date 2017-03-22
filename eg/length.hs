import Test.Speculate

-- needed for GHC <= 7.8, exported by Data.List on GHC >= 7.10.
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf []    _  = True
isSubsequenceOf (_:_) [] = False
isSubsequenceOf (x:xs) (y:ys)
  | x == y    =    xs  `isSubsequenceOf` ys
  | otherwise = (x:xs) `isSubsequenceOf` ys

main :: IO ()
main = speculate args
  { constants =
      [ background
      , showConstant ([] :: [Int])
      , constant ":"      $ (:)     -:>  int
      , constant "++"     $ (++)    -:> [int]
      , showConstant (0 :: Int)
--    , showConstant (1 :: Int)
      , foreground
      , constant "length" $ length  -:> [int]
      ]
  , instances = [ ordWith (isSubsequenceOf :: [Int] -> [Int] -> Bool) ]
  }
