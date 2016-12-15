{-# LANGUAGE TemplateHaskell #-}
import Speculate hiding (eval)
import Taut hiding (main)
import Test.LeanCheck

deriveListable ''Prop
deriveListable ''Name

prop :: Prop
prop = undefined

name :: Name
name = undefined

main :: IO ()
main = speculate args
  { typeInfo_ = typeInfo prop "p"
              : typeInfo name "n"
              : basicTypeInfo
  , showConditions = True
  , maxVars = 2
  , atoms =
      [ s False
      , s True
      , ((==) -:> prop) -| "=="

      , eval -| "eval"
      , varOf -| "varOf"
      , subst -| "subst"
      , taut -| "taut"
      ]
  }
