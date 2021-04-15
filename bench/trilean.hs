import Test.Speculate hiding ((|||), (&&&))
import Test.Speculate.Function.A10
import Test.LeanCheck.Function.ShowFunction

data Tril = M | F | T deriving (Show, Eq, Ord)

instance Listable Tril where
  tiers  =  cons0 M
         \/ cons0 F `addWeight` 1
         \/ cons0 T `addWeight` 1

instance ShowFunction Tril where bindtiers = bindtiersShow

instance Name Tril where
  name _  =  "p"

neg :: Tril -> Tril
neg T = F
neg F = T
neg M = M

unc :: Tril -> Tril
unc M = T
unc _ = F

(&&&) :: Tril -> Tril -> Tril
M &&& _  =  M
_ &&& M  =  M
T &&& T  =  T
_ &&& _  =  F

(|||) :: Tril -> Tril -> Tril
M ||| _  =  M
_ ||| M  =  M
F ||| F  =  F
_ ||| _  =  T

(===>) :: Tril -> Tril -> Tril
M ===> _  =  M
F ===> _  =  T
T ===> p  =  p

main :: IO ()
main = speculate args
  { constants =
      [ showConstant M
      , showConstant F
      , showConstant T
      , constant "neg" neg
--    , constant "unc" unc
      , constant "&&&" (&&&)
      , constant "|||" (|||)
--    , constant "===>" (===>)
      ]
  , instances = [ reifyInstances (undefined :: Tril)
--              , reifyInstances (undefined :: Tril -> Tril -> Tril)
                ]
  }
