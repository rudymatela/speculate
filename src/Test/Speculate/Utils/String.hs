-- |
-- Module      : Test.Speculate.Utils.String
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Speculate.
--
-- Utilities for manipulating strings.
module Test.Speculate.Utils.String
  ( module Data.String
  , module Data.Char
  , unquote
  , atomic
  , outernmostPrec
  , isNegativeLiteral
  , isInfix, isPrefix, isInfixedPrefix
  , toPrefix
  , prec
  , prime
  , indent, alignRight, alignLeft
  , splitAtCommas
  )
where

import Data.String
import Data.Char
import Data.Functor ((<$>)) -- for GHC < 7.10

unquote :: String -> String
unquote ('"':s) | last s == '"' = init s
unquote s = s

-- wrong but will work for a lot of cases
atomic :: String -> Bool
atomic s | all (not . isSpace) s = True
atomic ('\'':s) | last s == '\'' = True
atomic ('"':s)  | last s == '"'  = True
atomic ('[':s)  | last s == ']'  = True
atomic ('(':s)  | last s == ')'  = True
atomic _ = False

outernmostPrec :: String -> Maybe Int
outernmostPrec s =
  case words s of
    [l,o,r] | isInfix o -> Just (prec o)
    _                   -> Nothing

isNegativeLiteral :: String -> Bool
isNegativeLiteral s | not (atomic s) = False
isNegativeLiteral "-"                = False
isNegativeLiteral ('-':cs)           = all isDigit cs
isNegativeLiteral _                  = False

-- | Check if a function / operator is infix
--
-- > isInfix "foo"   == False
-- > isInfix "(+)"   == False
-- > isInfix "`foo`" == True
-- > isInfix "+"     == True
isInfix :: String -> Bool
isInfix (c:_) = c `notElem` "()'\"[" && not (isAlphaNum c)
isInfix "" = error "isInfix: empty string"

-- | Returns the precedence of default Haskell operators
prec :: String -> Int
prec " "  = 10
prec "!!" = 9
prec "."  = 9
prec "^"  = 8
prec "^^" = 8
prec "**" = 8
prec "*"  = 7
prec "/"  = 7
prec "%"  = 7
prec "+"  = 6
prec "-"  = 6
prec ":"  = 5
prec "++" = 5
prec "\\" = 5
prec ">"  = 4
prec "<"  = 4
prec ">=" = 4
prec "<=" = 4
prec "==" = 4
prec "/=" = 4
prec "`elem`" = 4
prec "&&" = 3
prec "||" = 2
prec ">>=" = 1
prec ">>" = 1
prec ">=>" = 1
prec "<=<" = 1
prec "$"  = 0
prec "`seq`" = 0
prec "==>" = 0
prec "<==>" = 0
prec _ = 9

isPrefix :: String -> Bool
isPrefix = not . isInfix

-- | Is the string of the form @`string`@
isInfixedPrefix :: String -> Bool
isInfixedPrefix s | not (atomic s) = False
isInfixedPrefix ('`':cs)           = last cs == '`'
isInfixedPrefix _                  = False

-- | Transform an infix operator into an infix function:
--
-- > toPrefix "`foo`" == "foo"
-- > toPrefix "+"     == "(+)"
toPrefix :: String -> String
toPrefix ('`':cs) = init cs
toPrefix cs = '(':cs ++ ")"

-- Primeify the name of a function by appending prime @'@ to functions and
-- minus @-@ to operators.
--
-- > prime "(+)"   == "(+-)"
-- > prime "foo"   == "foo'"
-- > prime "`foo`" == "`foo'`"
-- > prime "*"     == "*-
prime :: String -> String
prime ('`':cs) = '`':init cs ++ "'`" -- `foo` to `foo'`
prime ('(':cs) = '(':init cs ++ "-)" -- (+) to (+-)
prime cs | isInfix cs = cs ++ "-"    -- + to +-
         | otherwise  = cs ++ "'"    -- foo to foo'

alignRight :: Int -> String -> String
alignRight n cs = replicate (n - length cs) ' ' ++ cs

alignLeft :: Int -> String -> String
alignLeft n cs = cs ++ replicate (n - length cs) ' '

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

splitAtCommas :: String -> [String]
splitAtCommas = words . map commaToSpace
  where
  commaToSpace ',' = ' '
  commaToSpace  c  =  c
-- FIXME (uncomma): quick-and-dirty implementation
-- weird behaviour: uncomma "123 456,789" == ["123","456","789"]
-- but that's fine for speculate (Haskell symbols cannot have spaces)
