import Test.Speculate

import Data.Ratio

rational :: Rational
rational = undefined

main :: IO ()
main = speculate args
  { atoms =
      [ showConstant (0 :: Rational)
      , showConstant (1 :: Rational)
      , constant "id"          $ id          -:>  rational
      , constant "abs"         $ abs         -:>  rational
      , constant "negate"      $ negate      -:>  rational
      , constant "+"           $ (+)         -:>  rational
      , constant "*"           $ (*)         -:>  rational
      , constant "/"           $ (/)         -:>  rational
      , constant "%"           $ (%)         ->>: rational
      , constant "recip"       $ recip       -:>  rational
      , constant "numerator"   $ numerator   -:>  rational
      , constant "denominator" $ denominator -:>  rational
      ]
  }
