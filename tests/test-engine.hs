-- Test library
import Test

-- Functions under test
import Test.Speculate

-- Helper functions
import Test.Speculate.Reason (emptyThy)
import Test.Speculate.Utils
import Data.List
import Data.Maybe

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True
  
  , tiersExprTypeCorrect (n*2)

  , holds n $ \e -> mostGeneral  e == head (vassignments e)
  , holds n $ \e -> mostSpecific e == last (vassignments e)

  , vassignments (zero -+- xx) == [zero -+- xx]
  , vassignments (zero -+- i_) == [zero -+- xx]
  , vassignments (i_ -+- i_) == [xx -+- yy, xx -+- xx]
  , map canonicalize (vassignments (i_ -+- (i_ -+- ord' c_)))
    == [ xx -+- (yy -+- ord' cc)
       , xx -+- (xx -+- ord' cc) ]

  , vassignments (ii -+- i_) == [ii -+- xx]
  , map canonicalize (vassignments ((i_ -+- i_) -+- (ord' c_ -+- ord' c_)))
    == [ (xx -+- yy) -+- ((ord' cc) -+- (ord' dd))
       , (xx -+- yy) -+- ((ord' cc) -+- (ord' cc))
       , (xx -+- xx) -+- ((ord' cc) -+- (ord' dd))
       , (xx -+- xx) -+- ((ord' cc) -+- (ord' cc)) ]

  , holds n $ \e -> all (null . snd) (vars e)
                ==> let xs = map (length . vars) $ vassignments e
                    in (head xs >) `all` tail xs
                    && (last xs <) `all` init xs
  , holds n $ \e -> all null (map snd $ vars e)
                ==> unrepeatedVars (head (vassignments e))
  , holds n $ \e -> all (null . snd) (vars e)
                ==> let es = vassignments e
                    in (`isInstanceOf` head es) `all` tail es
                    && (last es `isInstanceOf`) `all` init es
  , holds n $ \e -> allLater (\e1 e0 -> not (e0 `isInstanceOf` e1))
                  $ vassignments e
  , holds n $ \e -> let es = vassignments e
                    in length (nub (sort es)) == length es
  , holds n $ \e -> length (vassignments e)
                 == product (map (bell . snd) . counts $ holes e)

  , equivalencesBetween (===) (i_ -+- i_) (i_ -+- i_)
    == [ ( xx -+- yy, yy -+- xx ) ]

  , equivalencesBetween (===) (i_ -+- (i_ -+- i_)) ((i_ -+- i_) -+- i_)
    == [ ( xx -+- (yy -+- zz), (xx -+- yy) -+- zz )
       , ( xx -+- (yy -+- zz), (xx -+- zz) -+- yy )
       , ( xx -+- (yy -+- zz), (yy -+- xx) -+- zz )
       , ( xx -+- (yy -+- zz), (yy -+- zz) -+- xx )
       , ( xx -+- (yy -+- zz), (zz -+- xx) -+- yy )
       , ( xx -+- (yy -+- zz), (zz -+- yy) -+- xx ) ]

  , holds n $ \(SameTypeE e1 e2) -> unEquation (phonyEquation e1 e2) == (e1,e2)

-- TODO: make the following pass (add ValueE and SameTypeValueE to Test)
--, holds n $ \(SameTypeE e1 e2) (SameTypeE e3 e4)
--         -> subConsequence emptyThy (e1 -==- e2) e3 e4
--         == subConsequence emptyThy (e2 -==- e1) e3 e4
--, holds n $ \(SameTypeE e1 e2) (SameTypeE e3 e4)
--         -> subConsequence emptyThy (e1 -==- e2) e3 e4
--         == subConsequence emptyThy (e1 -==- e2) e4 e3
--, holds n $ \...
--         -> not $ subConsequence emptyThy (e1 -<=- e2) e3 e4
  ,       subConsequence emptyThy [] (xx -==- yy) (xx -+- yy) (xx -+- xx)
  , not $ subConsequence emptyThy [] (xx -<=- yy) (xx -+- yy) (xx -+- xx)
  ,       subConsequence emptyThy [(xx -<=- yy, [xx -==- yy])]
                                     (xx -<=- yy) (xx -+- yy) (xx -+- xx)
  ,       subConsequence emptyThy [] (abs' xx -==- abs' yy) (abs' xx) (abs' yy)
  , not $ subConsequence emptyThy [] (abs' xx -<=- abs' yy) (abs' xx) (abs' yy)
  , not $ subConsequence emptyThy [] (abs' xx -==- one) (xx -+- abs' xx) (zero)
  ]
  where
  x === y = equal preludeInstances 1000 x y
