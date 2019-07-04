import Test.Speculate

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf []    _  = True
isSubsequenceOf (_:_) [] = False
isSubsequenceOf (x:xs) (y:ys)
  | x == y    =    xs  `isSubsequenceOf` ys
  | otherwise = (x:xs) `isSubsequenceOf` ys

main :: IO ()
main = speculate args
  { maxSemiSize = 0
  , maxVars     = 3
--, instances = [ mkOrdLessEqual (isPrefixOf      :: [Int] -> [Int] -> Bool) ]
--, instances = [ mkOrdLessEqual (isInfixOf       :: [Int] -> [Int] -> Bool) ]
  , instances = [ mkOrdLessEqual (isSubsequenceOf :: [Int] -> [Int] -> Bool) ]
  , constants =
      [ showConstant ([] :: [Int])
      , constant ":"    ((:)  ::  Int  -> [Int] -> [Int])
      , constant "++"   ((++) :: [Int] -> [Int] -> [Int])
      , constant "head" (head :: [Int] ->  Int)
      , constant "tail" (tail :: [Int] -> [Int])
--    , constant "null" (null :: [Int] -> Bool)
      ]
  }
