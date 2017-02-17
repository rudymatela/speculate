{-# LANGUAGE StandaloneDeriving #-}
import Test.Speculate hiding (match)
import Test.Speculate.Utils.Memoize
import Data.Function (on)
import Regex
import Data.Maybe (fromJust)

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

testMatches :: (Listable a, Show a, Charable a, Ord a) => RE a -> [Bool]
testMatches = tm `withMemory` mem
  where
  tm r = map (\e -> match toChar e r) $ take 120 list
  mem = memoryFor 720720 tm -- induces "Ord a" constraint

observingList :: (a -> a -> Bool) -> (b -> [a]) -> b -> b -> Bool
observingList g f = and .: (zipWith g `on` f) where (.:) = (.) . (.)

-- when running this, unless you set maxTests to 360
-- the following wrong law will appear:
-- r :. r <= r :+ s
-- when using -t360, better use 7207200 as memory for good measure
main :: IO ()
main = speculate args
  { maxTests = 30
  , maxSize = 4
  , instances =
      [ eqWith  $ ((==) `on` testMatches :: RE Symbol -> RE Symbol -> Bool)
      , ordWith $ ((<=) `observingList` testMatches :: RE Symbol -> RE Symbol -> Bool)
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
