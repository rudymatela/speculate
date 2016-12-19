{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
#if __GLASGOW_HASKELL__ == 704
import Test.Speculate -- go figure...
#else
import Test.Speculate hiding (value)
#endif
import Test.Speculate.Utils.Color
import Test.LeanCheck
import Data.Ratio

deriving instance Typeable Color -- for GHC < 7.10

-- Just for Listable.tiers enumeration
data ColorComponent = ColorComponent Rational

instance Listable ColorComponent where
  tiers = mapT (ColorComponent . uncurry (%))
        $ tiers `suchThat` (\(n,d) -> n >= 0 && d > 0 && n <= d && n `gcd` d == 1)
                `ofWeight` 0

instance Listable Color where
  tiers = cons3 (\(ColorComponent r) (ColorComponent g) (ColorComponent b) -> RGB r g b)

color :: Color
color = undefined

rational :: Rational
rational = undefined

main :: IO ()
main = speculate args
  { typeInfo_ = typeInfo color    "c"
              : typeInfo (mayb rational) "mq1"
              : basicTypeInfo
  , maxSize = 4
  , atoms =
      [ constant "black"     black
      , constant "white"     white
      , constant "red"       red
      , constant "green"     green
      , constant "blue"      blue
--    , constant "cyan"      cyan
--    , constant "magenta"   magenta
--    , constant "yellow"    yellow
--    , constant "orange"    orange
      , constant "+"       $ (+)    -:> color
      , constant "-"       $ (-)    -:> color
--    , constant "*"       $ (*)    -:> color
--    , constant "negate"  $ negate -:> color
--    , constant ".+."     $ (.+.)  -:> color
--    , constant ".-."     $ (.-.)  -:> color
--    , constant ".*."     $ (.*.)  -:> color
      , constant "chroma"     chroma
      , constant "hue"        hue
      , constant "saturation" saturation
--    , constant "intensity"  intensity
      , constant "value"      value
--    , constant "lightness"  lightness
      , constant "fromHSV"    fromHSV
      , constant "fromHSL"    fromHSL
      , constant "mix"        mix
--    , constant "mixHSV"     mixHSV

      , constant "Just"     $ Just -:> rational

      , showConstant (0 % 1 :: Rational)
      , showConstant (1 % 1 :: Rational)
      , showConstant (1 % 2 :: Rational)

--    , constant "==" $ (==) -:> color
--    , constant "/=" $ (/=) -:> color
--    , showConstant False
--    , showConstant True
--    , constant primary
      ]
  } 
