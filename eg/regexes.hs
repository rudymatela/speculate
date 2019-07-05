{-# Language CPP #-}
{-# Language DeriveDataTypeable, StandaloneDeriving #-}
import Test.Speculate
import Test.Speculate.Utils.Memoize
import Data.Function (on)
import Regex
import Data.Maybe (fromJust)

instance Listable Symbol where
  list = [Symbol 'a', Symbol 'b', Symbol 'c']

instance Listable a => Listable (RE a) where
  tiers = cons0 Empty
       \/ cons0 None
       \/ cons1 Lit
       \/ cons1 Star
       \/ cons2 (:+)
       \/ cons2 (:.)

instance Name Symbol where name _ = "c"
instance Name (RE a) where name _ = "r"

deriving instance Typeable Symbol
#if __GLASGOW_HASKELL__ < 708
deriving instance Typeable1 RE
#else
deriving instance Typeable RE
#endif

canonicalRE :: (Eq a, Ord a) => RE a -> Bool
-- by laws of size 3
canonicalRE (Star (Star r))        = False -- == Star r
canonicalRE (r      :+ s) | r >= s = False -- == r or == s :+ r
canonicalRE (r      :+ None)       = False -- == r
canonicalRE (None   :+ r)          = False -- == r
canonicalRE (r      :. Empty)      = False -- == r
canonicalRE (Empty  :. r)          = False -- == r
canonicalRE (r      :. None)       = False -- == None
canonicalRE (None   :. r)          = False -- == None
-- by laws of size 4
canonicalRE (r      :+ Star s) | r == s = False -- == Star r
canonicalRE (Star r :+ s)      | r == s = False -- == Star r
canonicalRE (Star r :. s)      | r == s = False -- == r :. Star r
canonicalRE (Star (r :+ Empty))         = False -- == Star r
canonicalRE (Star (Empty :+ r))         = False -- == Star r
canonicalRE (Empty  :+ Star r)          = False -- == Star r
canonicalRE (Star r :+ Empty)           = False -- == Star r
-- by laws of size 5
canonicalRE ((r :+ s) :+ t)               = False -- == r :+ (s :+ t)
canonicalRE ((r :. s) :. t)               = False -- == r :. (s :. t)
canonicalRE (Star (r :+ Star s))          = False -- == Star (r :+ s)
canonicalRE (Star (Star r :+ s))          = False -- == Star (r :+ s)
canonicalRE (r :. (s :+ Empty))           = False -- == r :+ (r :. s)
canonicalRE ((r :+ Empty) :. s)           = False -- == s :+ (r :. s)
canonicalRE (Star (r :. Star s)) | r == s = False -- == Star r
canonicalRE (Star (Star r :. s)) | r == s = False -- == Star r
canonicalRE (Star r :. Star s)   | r == s = False -- == Star r
-- by laws of size 6
canonicalRE (r :+ Star (r' :+ s))     | r == r'  =  False -- == Star (r :+ s)
canonicalRE (r :+ Star (s :+ r'))     | r == r'  =  False -- == Star (r :+ s)
canonicalRE (Star (r' :+ s) :+ r)     | r == r'  =  False -- == Star (r :+ s)
canonicalRE (Star (s :+ r') :+ r)     | r == r'  =  False -- == Star (r :+ s)
canonicalRE (Star r :. (r' :. s))     | r == r'  =  False -- == r :. (Star r :. s)
canonicalRE (Star (r :. s) :. r')     | r == r'  =  False -- == r :. Star (s :. r)
canonicalRE (r :+ (r' :. Star s))     | r == r'  =  False -- == r :. Star s
canonicalRE (r :+ (Star s :. r'))     | r == r'  =  False -- == Star s :. r
canonicalRE (Star (Star r :. Star s))            =  False -- == Star (r :+ s)
canonicalRE (Star (r :+ (r' :. r''))) | r == r' && r == r'' =  False -- == Star r
canonicalRE (Star r :+ (r' :. r''))   | r == r' && r == r'' =  False -- == Star r
canonicalRE (Empty :+ (r :. Star r')) | r == r'  =  False -- == Star r
-- default
canonicalRE _ = True



deriving instance Eq a => Eq (RE a)
deriving instance Ord a => Ord (RE a)

class    Charable a      where toChar :: a -> Char
instance Charable Char   where toChar = id
instance Charable Symbol where toChar (Symbol c) = c

testMatches :: (Listable a, Show a, Charable a, Ord a) => RE a -> [Bool]
testMatches = tm `withMemory` mem
  where
  tm r = map (\e -> match toChar e r) $ take 120 list
--mem = memoryFor 10000000 tm -- use when -t360
  mem = memoryFor 1000000 tm -- induces "Ord a" constraint

observingList :: (a -> a -> Bool) -> (b -> [a]) -> b -> b -> Bool
observingList g f = and .: (zipWith g `on` f) where (.:) = (.) . (.)

(/==/) :: RE Symbol -> RE Symbol -> Bool
(/==/) = (==) `on` testMatches

(/<=/) :: RE Symbol -> RE Symbol -> Bool
(/<=/) = (<=) `observingList` testMatches

-- wrong laws appear when setting maxSize to 5, even when tests are increased
-- to 500 (or more)
main :: IO ()
main = speculate args
  { maxTests = 30
  , maxSize = 4
  , instances =
      [ mkEq (/==/)
      , mkOrdLessEqual (/<=/)
      , reifyInstances (undefined :: Symbol)
      , reifyInstances (undefined :: RE Symbol)
      ]
  , constants =
      [ constant "Empty" (Empty :: RE Symbol)
      , constant "None"  (None  :: RE Symbol)
      , constant "Star"  (Star  :: RE Symbol -> RE Symbol)
      , constant ":+"    ((:+)  :: RE Symbol -> RE Symbol -> RE Symbol)
      , constant ":."    ((:.)  :: RE Symbol -> RE Symbol -> RE Symbol)
--    , constant "=~"    (=~)
--    , constant "Lit"   (Lit   :: Symbol -> RE Symbol)
      , background
      , constant "<=" (/<=/)
      , constant "==" (/==/)
      , showConstant False
      , showConstant True
      ]
  , showConditions    = False
  , force = True
  }
  where
  _ = (canonicalRE :: RE Symbol -> Bool) -- just to silence a warning
