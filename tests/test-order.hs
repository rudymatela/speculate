import Test
import Test.Speculate.Utils

import Test.Speculate.Expr
import Test.Speculate.Reason.Order

main :: IO ()
main = do
  n <- getMaxTestsFromArgs 10000

  reportTests (tests n)


tests :: Int -> [Bool]
tests n =
  [ True -- see test-expr.hs for general Expr orders

  , holds n $ simplificationOrder (|>|)
--, holds n $ simplificationOrder ( >|) -- TODO: make this pass, errors are making it fail
  , holds n $ simplificationOrder (|> )
  , holds n $ simplificationOrder (dwoBy (<))

  , fails n $ \e1 e2 -> (e1 |>| e2) == (e1 |>  e2)
  , fails n $ \e1 e2 -> (e1 |>| e2) == (e1  >| e2)
  , fails n $ \e1 e2 -> (e1 |>  e2) == (e1  >| e2)

  , not $ zero |> xx
  , not $ xx |> zero
  , negate' xx |> zero
  , negate' xx -+- xx |> zero
  , zero > xx
  , negateE > zero

  , weight xx                            == 1
  , weight zero                          == 1
  , weight (xx -+- zero)                 == 2
  , weight (one -+- yy)                  == 2
  , weight (xx -*- (yy -+- zz))          == 3
  , weight ((xx -*- yy) -+- (xx -*- zz)) == 4
  , holds n $ \e1 e2 -> weight (e1 -+- e2)  == weight e1 + weight e2
  , holds n $ \e     -> weight (e -+- zero) == 1 + weight e
  , holds n $ \e     -> weight (abs' e)     == 1 + weight e
  , holds n $ \e     -> weightExcept absE (abs' e)    <= weight e
  , holds n $ \e     -> weightExcept absE (negate' e) <= weight e + 1
  , holds n $ \e     -> weightExcept absE (abs' e)    == weightExcept absE e
  , holds n $ \e     -> weightExcept absE (negate' e) == weightExcept absE e + 1

  -- lexicompare is compatible (almost as if by coincidence)
  , fails n $ simplificationOrder lgt
  , holds n $ compatible          lgt
  , fails n $ closedUnderSub      lgt
  , fails n $ subtermProperty     lgt

  -- compare has the subtermProperty (Expr)
  , fails n $ simplificationOrder cgt
  , fails n $ compatible          cgt
  , fails n $ closedUnderSub      cgt
  , holds n $ subtermProperty     cgt
  ]
  where
  e1 `lgt` e2 = e1 `lexicompare` e2 == GT
  e1 `cgt` e2 = e1 `compare` e2 == GT

simplificationOrder :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Expr -> Bool
simplificationOrder (>) = \e1 e2 e3 -> reductionOrder  (>) e1 e2 e3
                                    && subtermProperty (>) e1

subtermProperty :: (Expr -> Expr -> Bool) -> Expr -> Bool
subtermProperty (>) = \e -> all (e >)
                          . filter (\e' -> e' /= e && typ e' == typ e)
                          $ subexprs e -- isn't this subexprsV? I don't think so

reductionOrder :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Expr -> Bool
reductionOrder (>) = \e1 e2 e3 -> strictPartialOrder (>) e1 e2 e3
                               && compatible         (>) e1 e2 e3
                               && closedUnderSub     (>) e1 e2 e3

compatible :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Expr -> Bool
compatible (>) = \e e1 e2 -> e1 > e2 && typ e1 == typ e2
                         ==> and [ (e //- [(v,e1)]) > (e //- [(v,e2)])
                                 | v <- vars e
                                 , typ v == typ e1
                                 , typ v == typ e2 ]

-- The formal definition contains multiple assignments,
-- here, just a single variable is assigned.
closedUnderSub :: (Expr -> Expr -> Bool) -> Expr -> Expr -> Expr -> Bool
closedUnderSub (>) = \e1 e2 e -> e1 > e2
                             ==> and [ (e1 //- [(v,e)]) > (e2 //- [(v,e)])
                                     | v <- vars e1 `nubMerge` vars e2
                                     , typ v == typ e ]
