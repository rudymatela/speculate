import Test.Speculate
import Data.List

main :: IO ()
main = speculate args
  { maxSemiSize = 0
  , maxVars     = 3
  , instances =
      [
--      ordWith (isPrefixOf -:> [int])
--      ordWith (isInfixOf  -:> [int])
--      ordWith ((>=)       -:> [int])
      ]
  , constants =
      [ showConstant ([] :: [Int])
      , constant ":"    $ (:)  -:>  int
      , constant "++"   $ (++) -:> [int]
      , constant "head" $ head -:> [int]
      , constant "tail" $ tail -:> [int]
--    , constant "null" $ null -:> [int]
      ]
  }
