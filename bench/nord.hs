import Test.Speculate
import Test.Speculate.Expr (name, listable, eq, ord)

data EqButNotOrd = C0 deriving (Eq, Show)

instance Listable EqButNotOrd where
  list = [C0]

-- EqButNotOrd is an instance of Eq but not of Ord,
-- Speculate reports a warning reflecting that.
main :: IO ()
main = speculate args
  { instances = [ name "x" C0
                , listable C0
                , eq C0
                ]
  , constants = [ showConstant C0
                , constant "id" (id :: EqButNotOrd -> EqButNotOrd)
                ]
  }
