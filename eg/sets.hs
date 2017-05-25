{-# Language CPP #-}
{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
#if __GLASGOW_HASKELL__ <= 704
import Test.Speculate hiding ((\/)) -- for some reason, Set is not exported
#else
import Test.Speculate hiding ((\/),Set)
#endif

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
  { instances = [ins "s" (set int)]
  , constants =
      [ constant "emptyS"      $ emptyS      -:  set int
      , constant "singleS"     $ singleS     -:> int    
    --, constant "pairS"       $ pairS       -:> int    
      , constant "insertS"     $ insertS     -:> int    
      , constant "deleteS"     $ deleteS     -:> int    
      , constant "sizeS"       $ sizeS       -:> set int
      , constant "<~"          $ (<~)        -:> int    
      , constant "\\/"         $ (\/)        -:> set int
      , constant "/\\"         $ (/\)        -:> set int
    --, constant "\\\\"        $ (\\)        -:> set int
    --, constant "<~"          $ (<~)        -:> set int
    --, constant "subS"        $ subS        -:> set int
    --, constant "powerS"      $ powerS      -:> set int
    --, constant "partitionsS" $ partitionsS -:> set int
      , background
      , showConstant False
      , showConstant True
      ]
  }
