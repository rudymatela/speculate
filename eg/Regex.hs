module Regex
  ( RE (..)
  )
where

import Text.Regex.TDFA

-- | Abstract representation of a Regular Expression.  This is a simple
--   abstraction supporting only the operations in a Kleene's algebra.
data RE a = Empty                -- Empty/"":      one
          | None                 -- Unmatchable:   zero
          | Lit a                -- Literals
          | Star (RE a)          -- Star/Asterisk: *
          | Choice (RE a) (RE a) -- Choice/Plus:   +
          | Append (RE a) (RE a) -- Append/Concat: .
  deriving Show
