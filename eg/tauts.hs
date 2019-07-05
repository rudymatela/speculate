{-# LANGUAGE TemplateHaskell #-}
{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
import Test.Speculate hiding (Name(..))
import qualified Test.Speculate as S
import Taut hiding (main)

deriveListable ''Prop
deriveListable ''Name

deriving instance Typeable Prop
deriving instance Typeable Name

prop :: Prop
prop = undefined

name :: Name
name = undefined

instance S.Name Prop where name _ = "p"
instance S.Name Name where name _ = "n"

main :: IO ()
main = speculate args
  { instances =
      [ reifyInstances prop
      , reifyInstances name
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
