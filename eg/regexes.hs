import Test.Speculate

import Regex

instance Listable Symbol where
  tiers = cons0 (Symbol 'a')
       \/ cons0 (Symbol 'b')
       \/ cons0 (Symbol 'c')

instance Listable a => Listable (RE a) where
  tiers = cons0 Empty
       \/ cons0 None
       \/ cons1 Lit
       \/ cons1 Star
       \/ cons2 (:+)
       \/ cons2 (:.)

main :: IO ()
main = speculate args
  { customTypeInfo =
      [ typeInfo (undefined :: Symbol)    "c"
      , typeInfo (undefined :: RE Symbol) "r"
      ]
  , constants =
      [ constant "=~"    (=~)
      , constant "Empty" (Empty :: RE Symbol)
      , constant "None"  (None  :: RE Symbol)
      , constant "Lit"   (Lit   :: Symbol -> RE Symbol)
      , constant "Star"  (Star  :: RE Symbol -> RE Symbol)
      , constant ":+"    ((:+)  :: RE Symbol -> RE Symbol -> RE Symbol)
      , constant ":."    ((:.)  :: RE Symbol -> RE Symbol -> RE Symbol)
      ]
  , showConditions       = False
  , showSemiequivalences = False
  }
