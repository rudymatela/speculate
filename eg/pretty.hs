{-# LANGUAGE CPP #-}
{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
import Speculate hiding (($$))
import Test.LeanCheck
import Data.Function (on)

import Text.PrettyPrint

deriving instance Data     Doc -- for GHC < 7.10
deriving instance Typeable Doc -- for GHC < 7.10

#if __GLASGOW_HASKELL__ < 710
-- pretty <= 1.1.1.1 (bundled with GHC <= 7.8)  does not provide this instance
-- pretty >= 1.1.2.0 (bundled with GHC >= 7.10) does provide this instance
instance Eq Doc where
  d == d' = show d == show d'
#endif

instance Ord Doc where
  compare = compare `on` show

instance Listable Doc where
  tiers = cons1 text

main :: IO ()
main = speculate args
  { typeInfo_ = typeInfo (undefined :: Doc) "d1"
              : basicTypeInfo
  , maxSize = 7
  , atoms = 
      [ ($$) -| "$$"
      , (<>) -| "<>"
      , nest -| "nest"
      , (++) -:> string -| "++"
      , length -:> string -| "length"
      ]
  }
