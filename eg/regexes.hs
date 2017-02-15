{-# LANGUAGE StandaloneDeriving #-}
import Test.Speculate hiding (match)
import Data.Function (on)
import Regex
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Map as M

instance Listable Symbol where
  tiers = cons0 (Symbol 'a')
       \/ cons0 (Symbol 'b')
       \/ cons0 (Symbol 'c')

instance Listable a => Listable (RE a) where
  tiers = cons0 Empty
       \/ cons0 None
       \/ cons1 Lit
       \/ cons1 Star
       \/ cons2 (:+)
       \/ cons2 (:.)

deriving instance Eq a => Eq (RE a)
deriving instance Ord a => Ord (RE a)

class    Charable a      where toChar :: a -> Char
instance Charable Char   where toChar = id
instance Charable Symbol where toChar (Symbol c) = c

testMatches :: (Listable a, Show a, Charable a) => RE a -> [Bool]
testMatches r = map (\e -> match toChar e r) $ take 100 list

testMatchesMemory :: (Listable a, Show a, Charable a, Ord a) => M.Map (RE a) [Bool]
testMatchesMemory = foldr (uncurry M.insert) M.empty $ take 1080 $ map rm list
  where
  rm r = (r, testMatches r)

memoTestMatches :: (Listable a, Show a, Charable a, Ord a) => RE a -> [Bool]
memoTestMatches r = fromMaybe (testMatches r) (M.lookup r testMatchesMemory)

main :: IO ()
main = speculate args
  { maxTests = 25
  , maxSize = 4
  , instances =
      [ eqWith  $ ((==) `on` memoTestMatches :: RE Symbol -> RE Symbol -> Bool)
      , ordWith $ ((<=) `on` memoTestMatches :: RE Symbol -> RE Symbol -> Bool)
      , ins "c" (undefined :: Symbol)
      , ins "r" (undefined :: RE Symbol)
      ]
  , constants =
      [ constant "Empty" (Empty :: RE Symbol)
      , constant "None"  (None  :: RE Symbol)
      , constant "Star"  (Star  :: RE Symbol -> RE Symbol)
      , constant ":+"    ((:+)  :: RE Symbol -> RE Symbol -> RE Symbol)
      , constant ":."    ((:.)  :: RE Symbol -> RE Symbol -> RE Symbol)
--    , constant "=~"    (=~)
--    , constant "Lit"   (Lit   :: Symbol -> RE Symbol)
      ]
  , showConditions    = False
  , force = True
  }
