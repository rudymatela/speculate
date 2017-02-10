module Test.Speculate.Utils.Misc where

import Test.LeanCheck
import Data.Maybe
import Data.List
import Data.Char
import Data.Function
import Data.Ratio
import Data.Tuple
import Test.Speculate.Utils.String
import Test.Speculate.Utils.List

-- easy debug:
undefined1 :: a
undefined1 = error "undefined1"

undefined2 :: a
undefined2 = error "undefined2"

-- TODO: Find a better name for iss:
--
-- > iss 0 0 = [ [] ]
-- > iss 0 1 = [ [0] ]
-- > iss 0 2 = [ [0,1], [0,0] ]
-- > iss 0 3 = [ [0,1,2], [0,1,0], [0,1,1], [0,0,1], [0,0,0] ]
iss :: Int -> Int -> [[Int]]
iss _ 0 = [[]]
iss i n = concat [map (j:) (iss (i + j-=-i) (n-1)) | j <- i:[0..(i-1)]]
  where x -=- y | x == y    = 1
                | otherwise = 0

thn :: Ordering -> Ordering -> Ordering
thn EQ o = o
thn o  _ = o
infixr 8 `thn`

-- TODO: Add a function like this on LeanCheck?  Not working exactly like
-- this, but something like:
--
-- > > classifyBy somefoo
-- > Int:    9988  99%
-- > Bool:     10   0%
-- > Char:      2   0%
-- > total: 10000 100%
--
-- > > preconditionInfo [precond_1, precond_2, precond_3]
-- > precond_1:    3   0%
-- > precond_2:  100   1%
-- > precond_3: 2345  23%
--
-- > > preconditionInfoT [...]
-- > tier           cond1      cond2      cond3
-- >   0    9       100 1%     303 2%     3821 4%
-- >   1   56       100 1%     303 2%     3821 4%
-- >   2  102       100 1%     303 2%     3821 4%
-- >   3  400       100 1%     303 2%     3821 4%
-- >   4  713       100 1%     303 2%     3821 4%
-- > all 1232       100 1%     303 2%     3821 4%
reportCountsBy :: (Eq b, Show b) => (a -> b) -> [a] -> IO ()
reportCountsBy f xs = putStrLn . unlines
                    . map showCount $ countsBy f xs
  where
  len = length xs
  showCount (x,n) = unquote (show x) ++ ": "
                 ++ show n ++ "/" ++ show len ++ " "
                 ++ show (100 * n `div` len) ++ "%"

-- O(1) bell number implementation (I'm lazy)
-- TODO: actually implement bell
bell :: Int -> Int
bell 0 = 1
bell 1 = 1
bell 2 = 2
bell 3 = 5
bell 4 = 15
bell 5 = 52
bell 6 = 203
bell 7 = 877
bell 8 = 4140
bell _ = error "bell: argument > 8, implement me!"

maybesToMaybe :: [Maybe a] -> Maybe a
maybesToMaybe = listToMaybe . catMaybes

maybe2 :: c -> (a -> b -> c) -> Maybe a -> Maybe b -> c
maybe2 _ f (Just x) (Just y) = f x y
maybe2 z _ _        _        = z

iterateUntil :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterateUntil p f x = let fx = f x
                     in if x `p` fx
                          then x
                          else iterateUntil p f (fx)

iterateUntilLimit :: Int -> (a -> a -> Bool) -> (a -> a) -> a -> a
iterateUntilLimit 0 p f x = x
iterateUntilLimit n p f x = let fx = f x
                            in if x `p` fx
                                 then x
                                 else iterateUntilLimit (n-1) p f (fx)

showRatio :: (Integral a, Show a) => Ratio a -> String
showRatio r = show (numerator r) ++ "/" ++ show (denominator r)

percent :: Integral a => Ratio a -> a
percent r = numerator r * 100 `div` denominator r

putLines :: [String] -> IO ()
putLines [] = return ()
putLines ls = putStrLn (unlines ls)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(..:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(..:) = (.) . (.:)
