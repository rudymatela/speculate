import Test.Speculate

import Data.Ratio

main :: IO ()
main = speculate args
  { constants =
      [ constant "id"          $ id          -:>  rational
      , constant "abs"         $ abs         -:>  rational
      , constant "negate"      $ negate      -:>  rational
      , constant "+"           $ (+)         -:>  rational
      , constant "*"           $ (*)         -:>  rational
      , constant "/"           $ (/)         -:>  rational
      , constant "%"           $ (%)         ->>: rational
      , constant "recip"       $ recip       -:>  rational
      , constant "numerator"   $ numerator   -:>  rational
      , constant "denominator" $ denominator -:>  rational
      , background
      , showConstant (0 :: Rational)
      , showConstant (1 :: Rational)
      , showConstant (0 :: Integer)
      , showConstant (1 :: Integer)
      ]
  }
