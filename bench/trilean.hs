-- bench/trilean.hs -- Speculate example on trileans
--
-- Copyright (C) 2021  Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
--
-- If used correctly,
-- Speculate deals with Trileans just fine producing correct equations.
--
--
-- The point of this example is to make Speculate produce incorrect equations
-- by failing to include all possible values on the 'Listable' enumeration.
--
-- Of the three possible trilean values (@M@, @F@ and @T@),
-- here we fail to enumerate @M@.  This ultimately causes Speculate print:
--
-- > p == M
--
-- How does that happen?
--
-- We first test to discover that:
--
-- > p &&& F == F
--
-- This is only true when x is F and T (the tested values)
-- but not true in general.
-- Speculate does not know this, then by consequence,
-- it infers:
--
-- > M &&& F == F
--
-- Which, using the correct equation @ M &&& p  ==  M @, rewrites to:
--
-- > M == F
--
-- Similarly we also get that:
--
-- > T == M
--
-- Ultimately arriving at:
--
-- > p == M
--
--
-- This illustrates why Speculate cannot consider equality between error values
-- using 'Test.LeanCheck.Error.errorToEither'
-- or    'Test.LeanCheck.Error.errorToNothing'.
--
-- You could also think now, to include error values in the generation,
-- however you will lose interesting equations if you do that
-- so it does not pay off.

import Test.Speculate hiding ((|||), (&&&))
import Test.Speculate.Function.A10
import Test.LeanCheck.Function.ShowFunction

data Tril = M | F | T deriving (Show, Eq, Ord)

instance Listable Tril where
  tiers  =  cons0 F
         \/ cons0 T
--       \/ cons0 M `ofWeight` 1
-- Here we intentionally fail to enumerate M.

instance ShowFunction Tril where bindtiers = bindtiersShow

instance Name Tril where
  name _  =  "p"

neg :: Tril -> Tril
neg T = F
neg F = T
neg M = M

unc :: Tril -> Tril
unc M = T
unc _ = F

(&&&) :: Tril -> Tril -> Tril
M &&& _  =  M
_ &&& M  =  M
T &&& T  =  T
_ &&& _  =  F

(|||) :: Tril -> Tril -> Tril
M ||| _  =  M
_ ||| M  =  M
F ||| F  =  F
_ ||| _  =  T

(===>) :: Tril -> Tril -> Tril
M ===> _  =  M
F ===> _  =  T
T ===> p  =  p

main :: IO ()
main = speculate args
  { constants =
      [ showConstant M
      , showConstant F
      , showConstant T
      , constant "neg" neg
      , constant "&&&" (&&&)
--    , constant "|||" (|||)
      ]
  , instances = [ reifyInstances (undefined :: Tril)
--              , reifyInstances (undefined :: Tril -> Tril -> Tril)
                ]
  }
