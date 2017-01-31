import Test.Speculate hiding (match)
import Data.Function (on)
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

class    Charable a      where toChar :: a -> Char
instance Charable Char   where toChar = id
instance Charable Symbol where toChar (Symbol c) = c

instance (Listable a, Show a, Charable a) => Eq (RE a) where
  r1 == r2 = holds 25 $ \e -> match toChar e r1 == match toChar e r2

instance (Listable a, Show a, Charable a) => Ord (RE a) where
  r1 `compare` r2 = error "not implemented"

main :: IO ()
main = speculate args
  { maxTests = 25
  , maxSize = 4
  , customTypeInfo =
      [ typeInfo (undefined :: Symbol)    "c"
      , typeInfo (undefined :: RE Symbol) "r"
      ]
  , constants =
      [ constant "Empty" (Empty :: RE Symbol)
      , constant "None"  (None  :: RE Symbol)
      , constant "Star"  (Star  :: RE Symbol -> RE Symbol)
      , constant ":+"    ((:+)  :: RE Symbol -> RE Symbol -> RE Symbol)
      , constant ":."    ((:.)  :: RE Symbol -> RE Symbol -> RE Symbol)
--    , constant "=~"    (=~)
--    , constant "Lit"   (Lit   :: Symbol -> RE Symbol)
      ]
  , showConditions       = False
  , showSemiequivalences = False
  }
