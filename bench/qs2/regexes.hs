import Regex
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Control.Monad
import QuickSpec hiding (None)
import Data.Ord

instance Arbitrary Symbol where
  arbitrary = elements $ map Symbol ['a','b','c']

instance Arbitrary a => Arbitrary (RE a) where
  arbitrary = sized re
    where
    re 0 = oneof 
         [ return Empty
         , return None
         , liftM Lit arbitrary
         ]
    re n = oneof
         [ re 0
         , liftM Star (re (n - 1))
         , liftM2 (:.) re2 re2
         , liftM2 (:+) re2 re2
         ] where re2 = re (n `div` 2)
  shrink Empty    = []
  shrink None     = []
  shrink (Lit _)  = [Empty, None]
  shrink (Star r) = [r] ++ [Star r' | r' <- shrink r]
  shrink (r :+ s) = [r, s] ++ [r' :+ s' | (r',s') <- shrink (r,s)]
  shrink (r :. s) = [r, s] ++ [r' :. s' | (r',s') <- shrink (r,s)]

class    Charable a      where toChar :: a -> Char
instance Charable Char   where toChar = id
instance Charable Symbol where toChar (Symbol c) = c

instance (Arbitrary a, Charable a, Ord a, Eq a) => Eq (RE a) where
  r1 == r2 = r1 `compare` r2 == EQ

instance (Arbitrary a, Charable a, Ord a) => Ord (RE a) where
  compare = comparing (\r -> map (\a -> match toChar a r) vals)
    where
  --vals :: Arbitrary a => [[a]] -- adapted from QuickSpec's own RE example
    vals = unGen (vector 100) (mkQCGen 12345) 10

main = quickSpec signature
  { maxTermSize = Just 2 -- TODO: fixme!
  , instances =
      [ baseType (undefined :: RE Symbol)
      ]
  , constants =
      [ constant "Empty" (Empty :: RE Symbol)
      , constant "None"  (None  :: RE Symbol)
      , constant "Star"  (Star  :: RE Symbol -> RE Symbol)
      , constant ":+"    ((:+)  :: RE Symbol -> RE Symbol -> RE Symbol)
      , constant ":."    ((:.)  :: RE Symbol -> RE Symbol -> RE Symbol)
    --, constant "=~"    (=~)
    --, constant "Lit"   (Lit   :: Symbol -> RE Symbol)
      ]
  }
