module Regex
  ( RE (..)
  , (=~)
  , match
  , Symbol (..)
  , stringToSymbols
  )
where

import qualified Text.Regex.TDFA as O

-- | Abstract representation of a Regular Expression.  This is a simple
--   abstraction supporting only the operations in a Kleene's algebra.
data RE a = Empty            -- Empty/"":      one
          | None             -- Unmatchable:   zero
          | Lit a            -- Literals
          | Star (RE a)      -- Star/Asterisk: *
          | (RE a) :+ (RE a) -- Choice/Plus:   +
          | (RE a) :. (RE a) -- Append/Concat: .
  deriving Show

-- | Compile an abstract regular expression to be used by Text.Regex
compile :: (a -> Char) -> RE a -> String
compile f r = "^" ++ c r ++ "$"
  where
  c Empty    = "()"
  c None     = "$u^" -- unmatchable
  c (Star r) = "(" ++ c r ++ ")*"
  c (r :+ s) = "(" ++ c r ++ "|" ++ c s ++ ")"
  c (r :. s) = c r ++ c s
  c (Lit c)  = [f c]

match :: (a -> Char) -> [a] -> RE a -> Bool
match f xs r = map f xs O.=~ compile f r


-- Now with symbols
newtype Symbol = Symbol Char deriving (Eq, Ord, Show)

(=~) :: [Symbol] -> RE Symbol -> Bool
(=~) = match symbolToChar
  where
  symbolToChar (Symbol c) = c

stringToSymbols :: String -> [Symbol]
stringToSymbols = map Symbol
