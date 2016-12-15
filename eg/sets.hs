{-# Language CPP #-}
{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
import Speculate
import Test.LeanCheck hiding ((\/))

import Set hiding (set)
import qualified Set

#if __GLASGOW_HASKELL__ < 708
deriving instance Typeable1 Set
#else
deriving instance Typeable Set -- for GHC < 7.10
#endif

instance (Ord a, Listable a) => Listable (Set a) where
  tiers = setCons Set.set

set :: a -> Set a
set = undefined

main :: IO ()
main = speculate args
  { typeInfo_ = typeInfo (set int) "s"
              : basicTypeInfo
  , atoms =
      [ False -| "False"
      , True  -| "True"
      , emptyS      -:  set int -| "emptyS" 
      , singleS     -:> int     -| "singleS"
    --, pairS       -:> int     -| "pairS"
      , insertS     -:> int     -| "insertS"
      , deleteS     -:> int     -| "deleteS"
      , sizeS       -:> set int -| "sizeS"
      , (<~)        -:> int     -| "<~"
      , (\/)        -:> set int -| "\\/"
      , (/\)        -:> set int -| "/\\"
    --, (\\)        -:> set int -| "\\\\"
    --, (<~)        -:> set int -| "<~"
    --, subS        -:> set int -| "subS"
    --, powerS      -:> set int -| "powerS"
    --, partitionsS -:> set int -| "partitionsS"
      ]
  }
