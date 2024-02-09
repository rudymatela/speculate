{-# LANGUAGE CPP #-}
-- Test library
import Test
import Test.Speculate.Utils

-- Functions under test
import Test.Speculate.Expr
import Test.Speculate.Reason
import Test.Speculate.Reason.Order
import Data.Tuple (swap)
import Data.Function (on)
import Data.List (permutations)

theorize' :: [(Expr,Expr)] -> Thy
theorize' eqs = finalize $ foldl (flip insert) emptyThy {closureLimit = 3, keepE = keepMaxOf eqs} eqs

theorize'' :: [(Expr,Expr)] -> Thy
theorize'' eqs = finalize $ foldr insert emptyThy {closureLimit = 3, keepE = keepMaxOf eqs} eqs

main :: IO ()
main = do
  mainTest tests 10000
  -- printStats

printStats :: IO ()
printStats  =  do
  n <- getMaxTestsFromArgs 10000

  putStrLn "\nlength (rules $ theorize eqs)"
  reportCountsBy (\(SameTypedPairsE eqs) -> show . length . rules $ theorize eqs)
                 (take n list)

  putStrLn "length (equations $ theorize eqs)"
  reportCountsBy (\(SameTypedPairsE eqs) -> show . length . equations $ theorize eqs)
                 (take n list)

  putStrLn "\\e1 e2 e3 -> e1 > e2 && typ e1 == typ e2,  length (vars e3)"
  reportCountsBy (\(e1,e2,e3) -> if e1 > e2 && typ e1 == typ e2
                                   then "OK, length " ++ show (length $ vars e3)
                                   else "Not OK")
                 (take n list)

  putStrLn "\\e e1 e2 -> length $ reductions 1 e (e1,e2)"
  reportCountsBy (\(e,e1,e2) -> if e1 > e2 && typ e1 == typ e2
                                  then "OK, length " ++ show (length $ reductions1 e (e1,e2))
                                  else "Not OK")
                 (take n list)

(~~) :: Expr -> Expr -> (Expr,Expr)
(~~) = (,)
infix 4 ~~

mkThy :: [(Expr,Expr)] -> [(Expr,Expr)] -> Thy
mkThy rs eqs = emptyThy
             { rules     = rs
             , equations = eqs }

tests :: Int -> [Bool]
tests n =
  [ True

  -- Listable Thy sound and complete:
  , holds n $ okThy
  , all (`elem` take n list) (take (n`div`10) listThyInefficient)

  , holds n $ \(SameTypedPairsE eqs) -> theorize eqs == theorize (map swap eqs)
  , holds n $ \(SameTypedPairsE eqs) -> theorize eqs == theorize (reverse eqs)

-- TODO: make the following two pass
-- > > let eqs = [(xx, id' xx), (zero, id' xx)]
-- > > theorize' eqs == theorize eqs
-- > False
-- > > theorize' eqs
-- > Thy { rules = [ id x == x
-- >               ]
-- >     , equations = [ 0 == x
-- >                   ]
-- >     , canReduceTo = (|>)
-- >     , closureLimit = 3
-- >     , keepE = keepUpToLength 3
-- >     }
-- > > theorize eqs
-- > Thy { rules = [ id x == x
-- >               , id x == 0
-- >               ]
-- >     , equations = [ 0 == x
-- >                   ]
-- >     , canReduceTo = (|>)
-- >     , closureLimit = 3
-- >     , keepE = keepUpToLength 3
-- >     }
--
-- This issue did exist before the introduction of Express,
-- it just wasn't found because Expr enumeration wasn't that good.
--, holds n $ \(SameTypedPairsE eqs) -> theorize'  eqs == theorize eqs
--, holds n $ \(SameTypedPairsE eqs) -> theorize'' eqs == theorize eqs

  , holds n $ okThy . deduce
  , holds n $ isIdempotent deduce
  , holds n $ \thy -> ((>=) `on` length . equations) (deduce thy) thy
  , holds n $ \thy -> ((==) `on` rules)              (deduce thy) thy

  , holds n $ okThy . simplify
  , holds n $ isIdempotent simplify
  , holds n $ \thy -> ((<=) `on` length . equations) (simplify thy) thy
  , holds n $ \thy -> ((==) `on` rules)              (simplify thy) thy

  , holds n $ okThy . delete
  , holds n $ isIdempotent delete
  , holds n $ \thy -> ((<=) `on` length . equations) (delete thy) thy
  , holds n $ \thy -> ((==) `on` rules)              (delete thy) thy

  , holds n $ okThy . orient
  , holds n $ isIdempotent orient
  , holds n $ \thy -> ((<=) `on` length . equations) (orient thy) thy
  , holds n $ \thy -> ((>=) `on` length . rules)     (orient thy) thy
  , holds n $ \thy -> length (equations thy)      - length (equations $ orient thy)
                   >= length (rules $ orient thy) - length (rules thy)

  , holds n $ okThy . compose
  , holds n $ isIdempotent compose
  , holds n $ \thy -> ((<=) `on` length . rules)     (compose thy) thy
  , holds n $ \thy -> ((==) `on` equations)          (compose thy) thy

  , holds n $ okThy . collapse
  , holds n $ isIdempotent collapse
  , holds n $ \thy -> ((<=) `on` length . rules)     (collapse thy) thy
  , holds n $ \thy -> ((>=) `on` length . equations) (collapse thy) thy

  , holds n $ okThy . complete . unThyght

  , holds n $ \(Thyght thy') (SameTypedPairsE eqs) ->
                let thy = complete thy'
                in  foldr insert thy eqs == complete (append thy eqs)

  -- TODO: make the following pass with n `div` 10
  -- Inference order should not matter:
  , holds 100
  $ \(Thyght thy) -> all (\steps -> iterateUntil (==) (chain steps) thy == complete thy)
                   $ permutations [collapse, compose, orient, delete . simplify, deduce]
  -- I now think the above property is not true in all cases, investigate.

  -- NOTE: the following does not hold in general, only for most of the cases
  , holds 4000
  $ \(Thyght thy') (SameTypeE e1 e2) -> closureLimit thy' > 0 ==>
       let thy = insert (e1,e2)
               $ thy' { keepE = keepUpToLength (max (size e1) (size e2)) }
       in  equivalent thy e1 e2

  , holds n $ isIdempotent finalize

  , criticalPairs emptyThy { rules = [ ((xx -+- yy) -+- zz,xx -+- (yy -+- zz))
                                     , (negate' xx -+- xx, zero) ] }
      == [ (negate' xx -+- (xx -+- yy),zero -+- yy)
         , ((xx -+- (yy -+- zz)) -+- xx', (xx -+- yy) -+- (zz -+- xx')) ]

  , criticalPairs emptyThy { rules = [ (negate' (negate' xx), id' xx) ] }
      == [ (negate' (id' xx), id' (negate' xx)) ]

  , criticalPairs emptyThy { rules = [ (foo (goo (foo xx)), xx)
                                     , (foo (goo xx), goo (foo xx)) ] }
      == [ (goo xx,                   foo (goo (goo (foo xx))))
         , (goo (foo xx),             foo (goo xx))
         , (goo (foo (foo xx)),       xx)
         , (goo (foo (foo (goo xx))), goo xx) ]

  , theorize [ (xx -*- yy) -*- (yy -*- zz)  ~~  yy ]
    |==|
    [ (xx -*- yy) -*- (yy -*- zz)  ~~  yy
    , xx -*- ((xx -*- yy) -*- zz)  ~~  xx -*- yy
    , (xx -*- (yy -*- zz)) -*- zz  ~~  yy -*- zz
    ] `mkThy` []

  , theorize [ xx -*- (yy -+- zz)  ~~  (xx -*- yy) -+- (xx -*- zz)
             , (xx -+- yy) -*- zz  ~~  (xx -*- zz) -+- (yy -*- zz) ]
    |==|
    [ (xx -*- yy) -+- (xx -*- zz)  ~~  xx -*- (yy -+- zz)
    , (xx -*- yy) -+- (zz -*- yy)  ~~  (xx -+- zz) -*- yy
    ] `mkThy` [ (xx -+- xx) -*- yy  ~~  xx -*- (yy -+- yy) ]

  , theorizeBy (|>|) [ xx -+- zero      ~~  xx
                     , xx -+- succ' yy  ~~  succ' (xx -+- yy) ]
    |==|
    [ xx -+- zero  ~~  xx
    ] `mkThy` [ xx -+- succ' yy  ~~  succ' (xx -+- yy) ]

  , theorizeBy (kboBy weight (<))
    [ xx -+- zero      ~~  xx
    , xx -+- succ' yy  ~~  succ' (xx -+- yy) ]
    |==|
    [                 xx -+- zero  ~~  xx
    , succ' (xx -+- yy)            ~~  xx -+- succ' yy
    , xx -+-         succ' zero    ~~         succ' xx
    , xx -+- (succ' (succ' zero))  ~~  succ' (succ' xx)
    ] `mkThy` []

  , theorizeBy (|>) [ xx -+- zero      ~~  xx
                    , xx -+- succ' yy  ~~  succ' (xx -+- yy) ]
    |==|
    [ xx -+- zero      ~~  xx
    , xx -+- succ' yy  ~~  succ' (xx -+- yy)
    ] `mkThy` []

  -- TODO: fix order under GHC <= 7.8
#if __GLASGOW_HASKELL >= 800
  , theorizeBy (dwoBy $ \e1 e2 -> if typ e1 == typ e2
                                      then e1 > e2
                                      else typ e1 < typ e2)
    [ ( xx -+- zero, xx )
    , ( xx -+- succ' yy, succ' (xx -+- yy) ) ]
    |==|
    [ ( xx -+- zero, xx )
    , ( succ' (xx -+- yy), xx -+- (succ' yy) )
    ] `mkThy` [ xx -+- (succ' zero)  ~~  succ' xx]
#endif

  , theorizeBy (|>) [ ( zero -+- xx, xx )
                    , ( negate' xx -+- xx, zero )
                    , ( (xx -+- yy) -+- zz, xx -+- (yy -+- zz) ) ]
    |==| [ ( zero -+- xx                , xx )
         , ( negate' xx -+- xx          , zero )
         , ( (xx -+- yy) -+- zz         , xx -+- (yy -+- zz) )
         , ( negate' xx -+- (xx -+- yy) , yy )
         , ( xx -+- zero                , xx )
         , ( xx -+- (negate' xx -+- yy) , yy )
         , ( negate' (negate' xx)         , xx )
         , ( negate' zero                 , zero )
         , ( xx -+- negate' xx            , zero )
         ] `mkThy` []

  , theorizeBy (kboBy (weightExcept negateE) (gtExcept (>) negateE))
      [ ( zero -+- xx, xx )
      , ( negate' xx -+- xx, zero )
      , ( (xx -+- yy) -+- zz, xx -+- (yy -+- zz) ) ]
    |==| [ ( zero -+- xx                , xx )
         , ( negate' xx -+- xx          , zero )
         , ( (xx -+- yy) -+- zz         , xx -+- (yy -+- zz) )
         , ( negate' xx -+- (xx -+- yy) , yy )
         , ( negate' zero -+- xx        , xx )
         , ( xx -+- zero                , xx )
         , ( xx -+- (negate' xx -+- yy) , yy )
         , ( negate' (negate' xx)         , xx )
         , ( xx -+- negate' xx            , zero )
         ] `mkThy` [( negate' zero, zero )]

  -- TODO: restore tests losts after removing test-kbc

  , groundJoinable aThy xx xx -- Eq case
  , groundJoinable aThy xx yy == False
  , groundJoinable aThy (ff2 xx yy) (ff2 xx yy) -- Eq case
  , groundJoinable aThy (xx -+- yy) (yy -+- xx) -- match case
  , groundJoinable aThy (xx -+- (yy -+- one)) ((yy -+- one) -+- xx) -- match case
  , groundJoinable aThy ((xx -+- yy) -*- zero) (zero -*- (yy -+- xx)) == False -- match case? no double-equation
  , groundJoinable aThy (zero -*- xx) (zero -*- yy) == False -- rewrite case?
  , groundJoinable aThy (xx -+- (yy -+- zz)) (zz -+- (xx -+- yy)) == False -- TODO: rewrite case
  ]

succ' :: Expr -> Expr
succ'  =  (value "succ" ((1+) :: Int -> Int) :$)

aThy :: Thy
aThy  =  theorize
      [  xx -+- zero ~~ xx
      ,  xx -*- one ~~ xx
      ,  xx -*- zero ~~ zero
      ,  zero -+- xx ~~ xx
      ,  one -*- xx ~~ xx
      ,  zero -*- xx ~~ zero
      ,  xx -+- yy ~~ yy -+- xx
      ,  xx -*- yy ~~ yy -*- xx
      ,  (xx -+- yy) -+- zz ~~ xx -+- (yy -+- zz)
      ,  (xx -*- yy) -*- zz ~~ xx -*- (yy -*- zz)
      ,  xx -*- (yy -+- yy) ~~ xx -*- yy -+- xx -*- yy
      ]
