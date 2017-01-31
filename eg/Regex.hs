module Regex
  ( RE (..)
  )
where

import Text.Regex.TDFA as O

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
compile :: (a -> String) -> RE a -> String
compile f r = "^" ++ c r ++ "$"
  where
  c Empty    = "()"
  c None     = "$u^" -- unmatchable
  c (Star r) = "(" ++ c r ++ ")*"
  c (r :+ s) = "(" ++ c r ++ "|" ++ c s ++ ")"
  c (r :. s) = c r ++ c s
  c (Lit c)  = f c
