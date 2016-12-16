import Test.Speculate

main :: IO ()
main = speculate args
  { showConditions = True
  , atoms =
      [ showConstant (0::Int)
      , showConstant (1::Int)
      , showConstant (2::Int)
      , showConstant False
      , showConstant True
      , constant "odd"    $ odd  -:> int
      , constant "even"   $ even -:> int
      , constant "`mod`"  $ mod  -:> int
      , constant "=="     $ (==) -:> int
      ]
{- after adding the following, conditions do not appear but in a way should:
  , conditionAtoms =
      [ constant "`mod`"  $ mod  -:> int
      , constant "=="     $ (==) -:> int
      ]
  , equationAtoms =
      [ constant "odd"    $ odd  -:> int
      , constant "even"   $ even -:> int
      ]
-- the problem is that "odd" and "even" are actually smaller versions of the
-- preconditions we "want":
-- > (x `mod` 2 == 0) == even x
-- conditionAtoms and equationAtoms should be used with care!
-- TODO: add that somewhere in the docs
-- TODO: make this not happen?  would be really hard: theorize twice!
-}
  }
