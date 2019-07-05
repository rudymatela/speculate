{-# LANGUAGE CPP #-}
{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
import Test.Speculate
import Data.Function (on)
import Data.List (isPrefixOf)

import Text.PrettyPrint as P

deriving instance Typeable Doc -- for GHC < 7.10

#if __GLASGOW_HASKELL__ < 710
-- pretty <= 1.1.1.1 (bundled with GHC <= 7.8)  does not provide this instance
-- pretty >= 1.1.2.0 (bundled with GHC >= 7.10) does provide this instance
instance Eq Doc where
  d == d' = show d == show d'
#endif

instance Ord Doc where
  (<=) = isPrefixOf `on` show

instance Listable Doc where
  tiers = cons1 text
       \/ cons2 ($$)
       \/ cons2 (P.<>)
       \/ cons2 nest

instance Name Doc where name _ = "d1"

main :: IO ()
main = speculate args
  { instances = [reifyInstances (undefined :: Doc)]
  , maxSize = 7
  , maxVars = 3
  , constants =
      [ constant "$$"       ($$)
      , constant "<>"       (P.<>)
      , constant "nest"     nest
      , background
      , constant "++"     $ (++) -:> string
      , constant "length" $ length -:> string
      ]
  }
