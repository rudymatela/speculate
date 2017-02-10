import Test.Speculate
import Data.List

main :: IO ()
main = speculate args
  { maxSemiSize = 0
  , maxVars     = 3
--, instances = [ ordWith (isPrefixOf      :: [Int] -> [Int] -> Bool) ]
--, instances = [ ordWith (isInfixOf       :: [Int] -> [Int] -> Bool) ]
--, instances = [ ordWith (isSubsequenceOf :: [Int] -> [Int] -> Bool) ]
  , constants =
      [ showConstant ([] :: [Int])
      , constant ":"    ((:)  ::  Int  -> [Int] -> [Int])
      , constant "++"   ((++) :: [Int] -> [Int] -> [Int])
      , constant "head" (head :: [Int] ->  Int)
      , constant "tail" (tail :: [Int] -> [Int])
--    , constant "null" (null :: [Int] -> Bool)
      ]
  }
