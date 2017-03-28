-- |
-- Module      : Test.Speculate.Utils.PrettyPrint
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- A very simple pretty printing library
module Test.Speculate.Utils.PrettyPrint
  ( beside
  , above
  , table
  , spaces
  )
where
-- TODO: Fix somewhat inefficient implementations, i.e.: heavy use of '(++)'.

import Data.List (intercalate,transpose,isSuffixOf)
import Data.Char (toUpper,isSpace)
import Test.Speculate.Utils.List
import Test.LeanCheck ((+|))

-- | Appends two Strings side by side, line by line
--
-- > beside ["asdf\nqw\n","zxvc\nas"] ==
-- >  "asdfzxvc\n\
-- >  \qw  as\n"
beside :: String -> String -> String
beside cs ds = unlines $ zipWith (++) (normalize ' ' css) dss
  where [css,dss] = normalize "" [lines cs,lines ds]

-- | Append two Strings on top of each other, adding line breaks *when needed*.
above :: String -> String -> String
above cs ds = if last cs == '\n' || head ds == '\n'
                then cs ++ ds
                else cs ++ '\n':ds

-- | Formats a table.  Examples:
--
-- > table "l  l  l" [ ["asdf", "qwer",     "zxvc\nzxvc"]
-- >                 , ["0",    "1",        "2"]
-- >                 , ["123",  "456\n789", "3"] ] ==
-- >   "asdf  qwer  zxvc\n\
-- >   \            zxvc\n\
-- >   \0     1     2\n\
-- >   \123   456   3\n\
-- >   \      789\n"
--
-- > table "r  l  l" [ ["asdf", "qwer",     "zxvc\nzxvc"]
-- >                 , ["0",    "1",        "2"]
-- >                 , ["123",  "456\n789", "3"] ] ==
-- >   "asdf  qwer  zxvc\n\
-- >   \            zxvc\n\
-- >   \   0  1     2\n\
-- >   \ 123  456   3\n\
-- >   \      789\n"
--
-- > table "r  r  l" [ ["asdf", "qwer",     "zxvc\nzxvc"]
-- >                 , ["0",    "1",        "2"]
-- >                 , ["123",  "456\n789", "3"] ] ==
-- >   "asdf  qwer  zxvc\n\
-- >   \            zxvc\n\
-- >   \   0     1  2\n\
-- >   \ 123   456  3\n\
-- >   \       789\n"
table :: String -> [[String]] -> String
table s []  = ""
table s sss = unlines
            . map (removeTrailing ' ')
            . map (concat . (+| spaces s))
            . transpose
            . zipWith (`normalizeTo` ' ') (discard  isSpace s)
            . foldr1 (zipWith (++))
            . map (normalize "" . map lines)
            . normalize ""
            $ sss

-- | Fits a list to a certain width by appending a certain value
--
-- > fit ' ' 6 "str" == "str   "
--
-- > fit 0 6 [1,2,3] == [1,2,3,0,0,0]
fit :: a -> Int -> [a] -> [a]
fit x n xs = xs ++ replicate (n - length xs) x

fitR :: a -> Int -> [a] -> [a]
fitR x n xs = replicate (n - length xs) x ++ xs

-- | normalize makes all list the same length by adding a value
--
-- > normalize ["asdf","qw","er"] == normalize ["asdf","qw  ","er  "]
normalize :: a -> [[a]] -> [[a]]
normalize x xs = map (x `fit` maxLength xs) xs

normalizeR :: a -> [[a]] -> [[a]]
normalizeR x xs = map (x `fitR` maxLength xs) xs

normalizeTo :: Char -> a -> [[a]] -> [[a]]
normalizeTo 'l' = normalize
normalizeTo 'r' = normalizeR

-- | Given a list of lists returns the maximum length
maxLength :: [[a]] -> Int
maxLength = maximum . (0:) . map length

removeTrailing :: Eq a => a -> [a] -> [a]
removeTrailing x = reverse
                 . dropWhile (==x)
                 . reverse

spaces :: String -> [String]
spaces "" = []
spaces s = case takeWhile isSpace s of
             "" ->      spaces (dropWhile isntSpace $ dropWhile isSpace s)
             s' -> s' : spaces (dropWhile isntSpace $ dropWhile isSpace s)

isntSpace :: Char -> Bool
isntSpace = not . isSpace
