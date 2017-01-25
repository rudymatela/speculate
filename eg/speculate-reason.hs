import Test.Speculate
import Test.Speculate.Reason
import Test

import Data.Function (on)
import Data.Monoid ((<>))

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
  , showConditions = False
  , showSemiequivalences = False
  , evalTimeout = Just 0.2
  , customTypeInfo =
      [ typeInfo (undefined :: Thyght)   "t"
      , typeInfo (undefined :: Equation) "eq"
      , typeInfo (undefined :: Expr)     "e"
      ]
  , constants =
      [ constant "okThy"      $ \(Thyght t)       -> okThy t

      , constant "insert"     $ \eq (Thyght t)    -> Thyght $ insert (unEquation eq) t
      , constant "complete"   $ \(Thyght t)       -> Thyght $ complete t
--    , constant "append"     $ \(Thyght t) eqs   -> Thyght $ append t $ map unEquation eqs

      , constant "normalize"  $ \(Thyght t) e     -> normalize t e
--    , constant "isNormal"      isNormal
      , constant "equivalent" $ \(Thyght t) e1 e2 -> equivalent t e1 e2

--    , constant "initialize"    initialize
--    , constant "theorize"      theorize
--    , constant "finalize"      finalize

--    , constant "criticalPairs" criticalPairs
--    , constant "normalizedCriticalPairs" normalizedCriticalPairs
--    , constant "difference"    difference
--    , constant "showThy"       showThy
      ]
  , backgroundConstants =
      [ constant "emptyThy" $ Thyght emptyThy
      , constant "True" True
      , constant "False" False
      ]
  } 
