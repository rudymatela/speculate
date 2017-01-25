import Test.Speculate
import Test.Speculate.Reason
import Test

import Data.Function (on)
import Data.Monoid ((<>))

instance Ord Thy where
  compare = (compare `on` rules)
         <> (compare `on` equations)
         <> (compare `on` closureLimit)

-- NOTE: we get wrong laws for size 5, but no wrong laws for size 4.
-- increasing the number of tests can get rid of those laws

main :: IO ()
main = speculate args
  { maxTests = 8000 -- one of the datatypes is too wide!
  , showConditions = False
  , showSemiequivalences = False
  , evalTimeout = Just 0.1
  , customTypeInfo =
      [ typeInfo (undefined :: Thy) "t"
      , typeInfo (undefined :: (Expr,Expr)) "eq"
      , typeInfo (undefined :: Expr) "e"
      ]
  , constants =
      [ constant "okThy"         okThy

      , constant "insert"        insert
      , constant "complete"      complete
      , constant "append"        append

      , constant "normalize"     normalize
--    , constant "isNormal"      isNormal
      , constant "equivalent"    equivalent

--    , constant "initialize"    initialize
--    , constant "theorize"      theorize
--    , constant "finalize"      finalize

--    , constant "criticalPairs" criticalPairs
--    , constant "normalizedCriticalPairs" normalizedCriticalPairs
--    , constant "difference"    difference
--    , constant "showThy"       showThy
      ]
  , backgroundConstants =
      [ constant "emptyThy" emptyThy
      , constant "True" True
      , constant "False" False
      ]
  } 
