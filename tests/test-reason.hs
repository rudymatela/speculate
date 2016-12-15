-- Test library
import Test
import Speculate.Utils

-- Functions under test
import Speculate
import Speculate.Reason
import Speculate.Reason.Order
import Data.Tuple (swap)
import Data.Function (on)
import Data.List (sortBy,permutations)

theorize' :: [(Expr,Expr)] -> Thy
theorize' eqs = finalize $ foldl (flip insert) emptyThy {closureLimit = 3, keepE = keepMaxOf eqs} eqs

theorize'' :: [(Expr,Expr)] -> Thy
theorize'' eqs = finalize $ foldr insert emptyThy {closureLimit = 3, keepE = keepMaxOf eqs} eqs

main :: IO ()
main = do
  n <- getMaxTestsFromArgs 10000

  reportTests (tests n)

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

  , holds n $ \(SameTypedPairsE eqs) -> theorize'  eqs == theorize eqs
  , holds n $ \(SameTypedPairsE eqs) -> theorize'' eqs == theorize eqs

  , holds n $ okThy . deduce
  , holds n $ idempotent deduce
  , holds n $ \thy -> ((>=) `on` length . equations) (deduce thy) thy
  , holds n $ \thy -> ((==) `on` rules)              (deduce thy) thy

  , holds n $ okThy . simplify
  , holds n $ idempotent simplify
  , holds n $ \thy -> ((<=) `on` length . equations) (simplify thy) thy
  , holds n $ \thy -> ((==) `on` rules)              (simplify thy) thy

  , holds n $ okThy . delete
  , holds n $ idempotent delete
  , holds n $ \thy -> ((<=) `on` length . equations) (delete thy) thy
  , holds n $ \thy -> ((==) `on` rules)              (delete thy) thy

  , holds n $ okThy . orient
  , holds n $ idempotent orient
  , holds n $ \thy -> ((<=) `on` length . equations) (orient thy) thy
  , holds n $ \thy -> ((>=) `on` length . rules)     (orient thy) thy
  , holds n $ \thy -> length (equations thy)      - length (equations $ orient thy)
                   >= length (rules $ orient thy) - length (rules thy)

  , holds n $ okThy . compose
  , holds n $ idempotent compose
  , holds n $ \thy -> ((<=) `on` length . rules)     (compose thy) thy
  , holds n $ \thy -> ((==) `on` equations)          (compose thy) thy

  , holds n $ okThy . collapse
  , holds n $ idempotent collapse
  , holds n $ \thy -> ((<=) `on` length . rules)     (collapse thy) thy
  , holds n $ \thy -> ((>=) `on` length . equations) (collapse thy) thy

  , holds n $ okThy . complete . unThyght

  , holds n $ \(Thyght thy') (SameTypedPairsE eqs) ->
                let thy = complete thy'
                in  foldr insert thy eqs == complete (append thy eqs)

  -- TODO: make the following pass with n `div` 10
  -- Inference order should not matter:
  , holds (n`div`100)
  $ \(Thyght thy) -> all (\steps -> iterateUntil (==) (chain steps) thy == complete thy)
                   $ permutations [collapse, compose, orient, delete . simplify, deduce]
  -- I now think the above property is not true in all cases, investigate.

  , holds n
  $ \(Thyght thy') (SameTypeE e1 e2) -> closureLimit thy' > 0 ==>
       let thy = insert (e1,e2)
               $ thy' { keepE = keepUpToLength (max (lengthE e1) (lengthE e2)) }
       in  equivalent thy e1 e2

  , holds n $ idempotent finalize

  , holds n $ \e (SameTypeE e1 e2) -> ordered (reductionsO e (e1,e2))

  , criticalPairs emptyThy { rules = [ ((xx -+- yy) -+- zz,xx -+- (yy -+- zz))
                                     , (negate' xx -+- xx, zero) ] }
      == [ (negate' xx -+- (xx -+- yy),zero -+- yy)
         , ((xx -+- (yy -+- zz)) -+- xx', (xx -+- yy) -+- (zz -+- xx')) ]

  , criticalPairs emptyThy { rules = [ (negate' (negate' xx), id' xx) ] }
      == [ (negate' (id' xx), id' (negate' xx)) ]

  , criticalPairs emptyThy { canReduceTo = dwoBy (\e1 e2 -> e1 `lexicompare` e2 == GT)
                           , rules = [ (ff (gg (ff xx)), xx)
                                     , (ff (gg xx), gg (ff xx)) ] }
      == (let sortuple (x,y) | x < y     = (y,x)
                             | otherwise = (x,y)
          in nubSort . map sortuple
           $ [ (gg xx,                ff (gg (gg (ff xx))))
             , (gg (ff xx),           ff (gg (xx)))
             , (gg (ff (ff xx)),      xx)
             , (gg (ff (ff (gg xx))), gg xx) ])

  , criticalPairs emptyThy { rules = [ (ff (gg (ff xx)), xx)
                                     , (ff (gg xx), gg (ff xx)) ] }
      == [ (gg (ff xx),           ff (gg (xx)))
         , (gg (ff (ff xx)),      xx)
         , (ff (gg (gg (ff xx))), gg xx)
         , (gg (ff (ff (gg xx))), gg xx) ]

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
    ] `mkThy` []

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
    , succ' (xx -+- yy)            ~~  xx -+- (succ' yy)
    , xx -+-        (succ' zero)   ~~          succ' xx
    , xx -+- (succ' (succ' zero))  ~~  succ' (succ' xx)
    ] `mkThy` []

  , theorizeBy (|>) [ xx -+- zero      ~~  xx
                    , xx -+- succ' yy  ~~  succ' (xx -+- yy) ]
    |==|
    [ xx -+- zero        ~~  xx
    , xx -+- (succ' yy)  ~~  succ' (xx -+- yy)
    ] `mkThy` []

  , theorizeBy (dwoBy $ \e1 e2 -> if typ e1 == typ e2
                                      then e1 > e2
                                      else typ e1 < typ e2)
    [ ( xx -+- zero, xx )
    , ( xx -+- succ' yy, succ' (xx -+- yy) ) ]
    |==|
    [ ( xx -+- zero, xx )
    , ( succ' (xx -+- yy), xx -+- (succ' yy) )
    ] `mkThy` [ xx -+- (succ' zero)  ~~  succ' xx]

  , theorizeBy (|>) [ ( zero -+- xx, xx )
                    , ( negate' xx -+- xx, zero )
                    , ( (xx -+- yy) -+- zz, xx -+- (yy -+- zz) ) ]
    |==| [ ( zero -+- xx                  , xx )
         , ( negate' xx -+- xx            , zero )
         , ( (xx -+- yy) -+- zz           , xx -+- (yy -+- zz) )
         , ( negate' xx -+- (xx -+- yy)   , yy )
         , ( xx -+- zero                  , xx )
         , ( xx -+- ((negate' xx) -+- yy) , yy )
         , ( negate' (negate' xx)         , xx )
         , ( negate' zero                 , zero )
         , ( xx -+- negate' xx            , zero )
         ] `mkThy` []

  , theorizeBy (kboBy (weightExcept negateE) (gtExcept (>) negateE))
      [ ( zero -+- xx, xx )
      , ( negate' xx -+- xx, zero )
      , ( (xx -+- yy) -+- zz, xx -+- (yy -+- zz) ) ]
    |==| [ ( zero -+- xx                  , xx )
         , ( negate' xx -+- xx            , zero )
         , ( (xx -+- yy) -+- zz           , xx -+- (yy -+- zz) )
         , ( negate' xx -+- (xx -+- yy)   , yy )
         , ( negate' zero -+- xx          , xx )
         , ( xx -+- zero                  , xx )
         , ( xx -+- ((negate' xx) -+- yy) , yy )
         , ( negate' (negate' xx)         , xx )
         , ( xx -+- negate' xx            , zero )
         ] `mkThy` [( negate' zero, zero )]

  -- TODO: restore tests losts after removing test-kbc
  ]
