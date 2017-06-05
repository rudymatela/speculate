{-# Language DeriveDataTypeable, StandaloneDeriving #-} -- for GHC <= 7.8
import Test.Speculate
import Test.Speculate.Expr
import Test.Speculate.Reason
import Test.Speculate.Utils.Timeout (timeoutToError)
import Test

import Data.Function (on)
import Data.Monoid ((<>))

-- for GHC <= 7.8
deriving instance Typeable Thyght
deriving instance Typeable Equation

instance Ord Thy where
  compare = (compare `on` rules)
         <> (compare `on` equations)
         <> (compare `on` closureLimit)

instance Eq Thyght where
  (==) = (==) `on` unThyght

instance Ord Thyght where
  compare = compare `on` unThyght

-- NOTE: we get wrong laws for size 5, but no wrong laws for size 4.
-- increasing the number of tests can get rid of those laws

main :: IO ()
main = speculate args
  { maxTests = 6000 -- one of the datatypes is too wide!
  , maxSize = 4
  , showConditions = False
  , showSemiequations = False
  , instances =
      [ ins "t"  (undefined :: Thyght)
      , ins "eq" (undefined :: Equation)
      , ins "e"  (undefined :: Expr)
      ]
  , constants =
      [ constant "okThy"      $ \(Thyght t)       -> okThy t

      , constant "insert"     $ \eq (Thyght t)    -> Thyght $ insert (unEquation eq) t
      , constant "complete"   $ \(Thyght t)       -> Thyght $ complete t -- $ timeoutToError 0.2 $ complete t

      , constant "normalize"  $ \(Thyght t) e     -> normalize t e
      , constant "equivalent" $ \(Thyght t) e1 e2 -> equivalent t e1 e2

      , constant "append"     $ \(Thyght t) eqs   -> Thyght $ append t $ map unEquation eqs
--    , constant "isNormal"      isNormal

--    , constant "initialize"    initialize
--    , constant "theorize"      theorize
--    , constant "finalize"      finalize

--    , constant "criticalPairs" criticalPairs
--    , constant "normalizedCriticalPairs" normalizedCriticalPairs
--    , constant "difference"    difference
--    , constant "showThy"       showThy

      , background
      , constant "emptyThy" $ Thyght emptyThy
      , constant "True" True
      , constant "False" False
      , constant "Equation" $ Equation
      ]
  } 
