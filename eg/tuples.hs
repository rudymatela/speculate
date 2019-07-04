import Test.Speculate
import Data.Tuple

right :: (a,b,c) -> (c,a,b)
right (x,y,z) = (z,x,y)

left :: (a,b,c) -> (b,c,a)
left (x,y,z) = (y,z,x)

main :: IO ()
main = speculate args
  { maxSemiSize = 0
  , maxVars     = 3
  , instances = [ reifyInstances (int,int,int) ]
  , constants =
      [ showConstant (() :: ())
      , constant "id"    (id :: () -> ())
      , constant "id"    (id :: Int -> Int)
      , constant "id"    (id :: (Int,Int) -> (Int,Int))
      , constant "id"    (id :: (Int,Int,Int) -> (Int,Int,Int))
      , constant ","     ((,) :: Int -> Int -> (Int,Int))
      , constant ",,"    ((,,) :: Int -> Int -> Int -> (Int,Int,Int))
      , constant "fst"   (fst :: (Int,Int) -> Int)
      , constant "snd"   (snd :: (Int,Int) -> Int)
      , constant "swap"  (swap :: (Int,Int) -> (Int,Int))
      , constant "right" (right :: (Int,Int,Int) -> (Int,Int,Int))
      , constant "left"  (left  :: (Int,Int,Int) -> (Int,Int,Int))
      ]
  }
