import Test.Speculate
import Test.Speculate.Reason
import Test

import Data.Function (on)
import Data.Monoid ((<>))

instance Ord Thy where
  compare = (compare `on` rules)
         <> (compare `on` equations)
         <> (compare `on` closureLimit)

warnWrong :: IO () -> IO ()
warnWrong action = do
  putStrLn "WARNING: most laws below are not correct (see source)"
  action
  putStrLn "WARNING: most laws above are not correct (see source)"

-- WARNING: this example right now is not correct as the number of tests is too
-- low.  When the number of tests is > 1406, some combination of functions and
-- values cause an infinite loop for the reason library.
-- Possible fixes:
--   * add a timeout to the Speculate eval function (laborious);
--   * make Reason always terminate (hard);
--   * remove some functions from constants (probably won't work).
--
-- To run this, it is better to change (on Listable Thy from Test.hs):
-- 
--   $ concatMapT expandKeepE
--
-- to
--
--   $ mapT defaultKeep

main :: IO ()
main = warnWrong $ speculate args
  { maxTests = 1406 -- 1407 infinite loop?
  , showConditions = False
  , showSemiequivalences = False
  , maxSize = 3
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
      , constant "isNormal"      isNormal
      , constant "equivalent"    equivalent

      , constant "initialize"    initialize
--    , constant "theorize"      theorize
      , constant "finalize"      finalize

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
