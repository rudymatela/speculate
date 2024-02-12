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
  , indent
  , alignRight
  , alignLeft
  , splitAtCommas
  )
where

import Data.String
import Data.Char
import Data.Express.Utils.String

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
