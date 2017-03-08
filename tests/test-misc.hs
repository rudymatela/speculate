-- Test library
import Test

-- Functions under test
import Test.Speculate.Misc

-- Utils
import Test.Speculate.Expr
import Test.Speculate.Utils

main :: IO ()
main = mainTest tests 10000

-- NOTE: remember that in the expressions returned by functions<N> the
-- variables are actually values (bound by the arguments of the returned
-- function)

tests :: Int -> [Bool]
tests n =
  [ True
  , fillings (ord' c_) [cc,dd,xx] == [ord' cc, ord' dd]

  , map (show *** ($1))                  (functions1 (i_ -+- i_) :: [(Expr,Int->Int)])
    == [("x + x :: Int", 2)]

  , map (show *** (($1).($2)))           (functions2 (i_ -+- i_) :: [(Expr,Int->Int->Int)])
    == [ ("x + x :: Int", 4)
       , ("x + y :: Int", 3)
       , ("y + x :: Int", 3)
       , ("y + y :: Int", 2) ]

  , map (show *** (($1).($2).($3)))      (functions3 (i_ -+- i_) :: [(Expr,Int->Int->Int->Int)])
    == [ ("x + x :: Int",6)
       , ("x + y :: Int",5)
       , ("y + x :: Int",5)
       , ("x + z :: Int",4)
       , ("y + y :: Int",4)
       , ("z + x :: Int",4)
       , ("y + z :: Int",3)
       , ("z + y :: Int",3)
       , ("z + z :: Int",2) ]

  , map (show *** (($1).($2).($3).($4))) (functions4 (i_ -+- i_) :: [(Expr,Int->Int->Int->Int->Int)])
    == [ ("x + x :: Int",8)
       , ("x + y :: Int",7)
       , ("y + x :: Int",7)
       , ("x + z :: Int",6)
       , ("y + y :: Int",6)
       , ("z + x :: Int",6)
       , ("x + w :: Int",5)
       , ("y + z :: Int",5)
       , ("z + y :: Int",5)
       , ("w + x :: Int",5)
       , ("y + w :: Int",4)
       , ("z + z :: Int",4)
       , ("w + y :: Int",4)
       , ("z + w :: Int",3)
       , ("w + z :: Int",3)
       , ("w + w :: Int",2) ]

  , take 3 (expressionsOf [ zero, xx, absE, plusE ])
    == [ [ zero, xx, absE, plusE ]
       , [ abs' zero, abs' xx, plusE :$ zero, plusE :$ xx ]
       , [ abs' (abs' zero)
         , abs' (abs' xx)
         , plusE :$ abs' zero
         , plusE :$ abs' xx
         , zero -+- zero
         , zero -+- xx
         , xx -+- zero
         , xx -+- xx ] ]

  , take 3 (valuedExpressionsOf [ zero, one, absE, plusE ])
    == [ [ (zero, 0 :: Int)
         , (one,  1) ]
       , [ (abs' zero, 0)
         , (abs' one,  1) ]
       , [ (abs' (abs' zero), 0)
         , (abs' (abs' one),  1)
         , (zero -+- zero, 0)
         , (zero -+- one,  1)
         , (one -+- zero,  1)
         , (one -+- one,   2) ] ]

  , take 3 (mapT (id *** ($ (10::Int))) $ valuedExpressionsOf [ zero, one, absE, plusE ])
    == [ [ (absE, 10 :: Int) ]
       , [ (plusE :$ zero, 10)
         , (plusE :$ one,  11) ]
       , [ (plusE :$ abs' zero, 10)
         , (plusE :$ abs' one,  11) ] ]
  ]
