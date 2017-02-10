{-# LANGUAGE TemplateHaskell #-}
{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
import Test.Speculate hiding (eval)
import Taut hiding (main)
import Test.LeanCheck

deriveListable ''Prop
deriveListable ''Name

deriving instance Typeable Prop
deriving instance Typeable Name

prop :: Prop
prop = undefined

name :: Name
name = undefined

main :: IO ()
main = speculate args
  { instances =
      [ ins "p" prop
      , ins "n" name
      ]
  , showConditions = True
  , maxVars = 2
  , maxTests = 4000
  , constants =
      [ showConstant False
      , showConstant True
      , constant "==" ((==) -:> prop)

      , constant "eval"  eval
      , constant "varOf" varOf
      , constant "subst" subst
      , constant "taut"  taut
      ]
  }
