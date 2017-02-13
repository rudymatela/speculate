-- Test library
import Test

-- Functions under test
import Test.Speculate.Utils

-- Helper functions
import Data.List
import Data.Function

import Test.LeanCheck.Utils (comparison)

main :: IO ()
main = mainTest tests 10000

tests :: Int -> [Bool]
tests n =
  [ True
  
  -- iss is sound
  , and [ length xs == n     | m <- [0..5], n <- [0..5], xs <- iss m n ]
  , and [ head (iss 0 n) == [0..(n-1)]    | n <- [0..10] ]
  , and [ last (iss 0 n) == replicate n 0 | n <- [0..10] ]
  , and [ firsts xs `isPrefixOf` [0..]    | n <- [0..10], xs <- iss 0 n ]
  , holds 100 $ \xs ys -> strictlyOrdered xs && strictlyOrdered ys
                      ==> strictlyOrdered (nubMerge xs (ys::[Int]))
  , and [ nubMerge xs xs == xs | n <- [0..10], let xs = [0..n] ]

  , holds 100 $ \xss -> ordered . concat $ discardLaterT (<=) (xss::[[Int]])
  , (length . concat $ discardLaterT (<=) [[1..100]]) == 100

  , collectOn snd [(1,2),(2,2),(2,3),(3,3)] == [[(1,2),(2,2)],[(2,3),(3,3)]]
  , collectOn fst [(1,2),(2,2),(2,3),(3,3)] == [[(1,2)],[(2,2),(2,3)],[(3,3)]]
  , collectOn (uncurry (+)) [(1,2),(2,2),(2,3),(3,3)] == [[(1,2)],[(2,2)],[(2,3)],[(3,3)]]
  , collectBy (compare `on` snd) [(1,2),(2,2),(2,3),(3,3)] == [[(1,2),(2,2)],[(2,3),(3,3)]]

  , holds n $ \x           -> medianate (,) [x::Int]           == [(x,x)]
  , holds n $ \x y         -> medianate (,) [x::Int,y]         == [(x,y)]
  , holds n $ \x y z       -> medianate (,) [x::Int,y,z]       == [(x,z),(y,y)]
  , holds n $ \x y z w     -> medianate (,) [x::Int,y,z,w]     == [(x,w),(y,z)]
  , holds n $ \x y z w v   -> medianate (,) [x::Int,y,z,w,v]   == [(x,v),(y,w),(z,z)]
  , holds n $ \x y z w v u -> medianate (,) [x::Int,y,z,w,v,u] == [(x,u),(y,v),(z,w)]

  , all (\f -> discardOthers f [] == ([]::[Int])) [(<=),(>=),(<),(>),(==),(/=),\x y -> True]
  , holds n $ \x -> all (\f -> discardOthers f [x] == [x::Int])
                        [(<=),(>=),(<),(>),(==),(/=),\x y -> True]
  , holds n $ \xs -> not (null xs) ==> discardOthers (<=) xs == [maximum xs::Int]
  , holds n $ \xs -> not (null xs) ==> discardOthers (>=) xs == [minimum xs::Int]
  , holds n $ \xs -> not (null xs) ==>
      discardOthers (<) xs == replicate (count (maximum xs) xs) (maximum xs::Int)
  , holds n $ \xs -> not (null xs) ==>
      discardOthers (>) xs == replicate (count (minimum xs) xs) (minimum xs::Int)

  , ["x","y","z","x'","y'"]    `isPrefixOf` namesFromTemplate "x"
  , ["xs","ys","zs","xs'"]     `isPrefixOf` namesFromTemplate "xs"
  , ["xss","yss","zss","xss'"] `isPrefixOf` namesFromTemplate "xss"
  , ["c","d","e","c'","d'"]    `isPrefixOf` namesFromTemplate "c"
  , ["s","t","u","s'","t'"]    `isPrefixOf` namesFromTemplate "s"
  , ["0","1","2","3","4"]      `isPrefixOf` namesFromTemplate "0"
  , ["1","2","3","4","5"]      `isPrefixOf` namesFromTemplate "1"
--, ["z","a","b","z'","a'"]    `isPrefixOf` namesFromTemplate "z" -- TODO: make this pass
  , ["x1","x2","x3","x4"]      `isPrefixOf` namesFromTemplate "x1"
  , ["a0","a1","a2","a3"]      `isPrefixOf` namesFromTemplate "a0"
  , ["e1","e2","e3","e4"]      `isPrefixOf` namesFromTemplate "e1"
  , ["xs1","xs2","xs3"]        `isPrefixOf` namesFromTemplate "xs1"
  , ["xy","zw","xy'","zw'"]    `isPrefixOf` namesFromTemplate "xy"
  , ["ab","cd","ab'","cd'"]    `isPrefixOf` namesFromTemplate "ab"
  , ["xys","zws","xys'"]       `isPrefixOf` namesFromTemplate "xys"
  , ["xyz","xyz'","xyz''"]     `isPrefixOf` namesFromTemplate "xyz"

  , table "l  l  l" [ ["asdf", "qwer",     "zxvc\nzxvc"]
                    , ["0",    "1",        "2"]
                    , ["123",  "456\n789", "3"] ] ==
      "asdf  qwer  zxvc\n\
      \            zxvc\n\
      \0     1     2\n\
      \123   456   3\n\
      \      789\n"

  , table "r  l  l" [ ["asdf", "qwer",     "zxvc\nzxvc"]
                    , ["0",    "1",        "2"]
                    , ["123",  "456\n789", "3"] ] ==
      "asdf  qwer  zxvc\n\
      \            zxvc\n\
      \   0  1     2\n\
      \ 123  456   3\n\
      \      789\n"

  , table "r  r  l" [ ["asdf", "qwer",     "zxvc\nzxvc"]
                    , ["0",    "1",        "2"]
                    , ["123",  "456\n789", "3"] ] ==
      "asdf  qwer  zxvc\n\
      \            zxvc\n\
      \   0     1  2\n\
      \ 123   456  3\n\
      \       789\n"

  , show (typesIn $ typeOf (undefined :: (Int -> Int) -> Int -> Bool))
      == "[Int,Bool]"
  , show (typesIn $ typeOf (undefined :: (Int -> Char) -> Integer -> Bool))
      == "[Integer,Int,Char,Bool]"

  , splitAtCommas "1,2,3" == ["1","2","3"]
  , splitAtCommas "123,456,789," == ["123","456","789"]
  , splitAtCommas "123 456,789"  == ["123","456","789"] -- weird behaviour, but fine for speculate

  , compareIndex [3,2,1] 3 1 == LT
  , compareIndex [3,2,1] 1 3 == GT
  , compareIndex [3,2,1] 4 1 == GT
  , compareIndex [3,2,1] 1 0 == LT
  , holds n $ \xs -> comparison (compareIndex (xs::[Int]))
  ]
