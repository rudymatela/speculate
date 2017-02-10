import Test.Speculate
import Test.Speculate.Utils (primeCycle)
import System.Environment (getArgs)

-- How well does this tool performs on artificial operators
f :: Num a => a -> a -> a
f x y = x*17+23

g :: Num a => a -> a -> a
g x y = 42

h :: Num a => a -> a -> a
h x y = y*13+19

main :: IO ()
main = speculate args
  { instances = [ins "x" int]
  , constants =
      [ hole int
      , showConstant (0::Int)
      , constant "id" $ id  -:> int
      , constant "+"  $ (+) -:> int
      , constant "f"  $ f   -:> int
      , constant "g"  $ g   -:> int
      , constant "h"  $ h   -:> int
      ]
  }
