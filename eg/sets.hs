{-# Language CPP #-}
{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
import Test.Speculate hiding ((\/))

import Set hiding (set)
import qualified Set as S

#if __GLASGOW_HASKELL__ < 708
deriving instance Typeable1 S.Set
#else
deriving instance Typeable S.Set -- for GHC < 7.10
#endif

instance (Ord a, Listable a) => Listable (S.Set a) where
  tiers = setCons S.set

instance Name (S.Set a) where name _ = "s"

set :: a -> S.Set a
set = undefined

main :: IO ()
main = speculate args
  { instances = [reifyInstances (set int)]
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
