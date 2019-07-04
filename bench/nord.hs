{-# LANGUAGE CPP, DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
import Test.Speculate
import Test.Speculate.Expr (listable)

#if __GLASGOW_HASKELL__ < 710
import Data.Typeable (Typeable1)
deriving instance Typeable EqButNotOrd
#endif

data EqButNotOrd = C0 deriving (Eq, Show)

instance Listable EqButNotOrd where
  list = [C0]

-- EqButNotOrd is an instance of Eq but not of Ord,
-- Speculate reports a warning reflecting that.
main :: IO ()
main = speculate args
  { instances = [ listable C0
                , reifyEq C0
                ]
  , constants = [ showConstant C0
                , constant "id" (id :: EqButNotOrd -> EqButNotOrd)
                ]
  }
