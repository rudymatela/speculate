{-# LANGUAGE StandaloneDeriving #-}
import Regex
import Test.QuickCheck
import Control.Monad
import QuickSpec hiding (None)

deriving instance Eq a  => Eq  (RE a)
deriving instance Ord a => Ord (RE a)

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

main = quickSpec signature
  { maxTermSize = Just 5
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
