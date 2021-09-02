import Test.Speculate
import Data.List (sort)

-- needed for GHC <= 7.8, exported by Data.List on GHC >= 7.10.
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf []    _  = True
isSubsequenceOf (_:_) [] = False
isSubsequenceOf (x:xs) (y:ys)
  | x == y    =    xs  `isSubsequenceOf` ys
  | otherwise = (x:xs) `isSubsequenceOf` ys

main :: IO ()
main = do
  speculate args
    { constants = [ constant "sort" (sort :: [Int] -> [Int])
                  , constant "`isSubsequenceOf`" (isSubsequenceOf :: [Int] -> [Int] -> Bool)
                  ]
    }

  -- a low number of tests may produce incorrect equations
  speculate args
    { maxTests = 60
    , constants = [ constant "sort" (sort :: [Int] -> [Int])
                  , constant "`isSubsequenceOf`" (isSubsequenceOf :: [Int] -> [Int] -> Bool)
                  ]
    }
