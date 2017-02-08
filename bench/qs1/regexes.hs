{-# LANGUAGE StandaloneDeriving #-}
import Regex
import Test.QuickCheck
import Control.Monad
import Test.QuickSpec

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

main = quickSpec
  [ ["r1", "r2", "r3"] `vars` (undefined :: RE Symbol)
  , "Empty" `fun0` (Empty :: RE Symbol)
  , "None"  `fun0` (None  :: RE Symbol)
  , "Star"  `fun1` (Star  :: RE Symbol -> RE Symbol)
  , ":+"    `fun2` ((:+)  :: RE Symbol -> RE Symbol -> RE Symbol)
  , ":."    `fun2` ((:.)  :: RE Symbol -> RE Symbol -> RE Symbol)
--, "=~"    `fun2` (=~)
--, "Lit"   `fun2` (Lit   :: Symbol -> RE Symbol)
  , observer2 (=~)
  , withDepth 3
  , withSize  100
  , withTests 500
  , withQuickCheckSize 20
  ]
