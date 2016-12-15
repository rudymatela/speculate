{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
#if __GLASGOW_HASKELL__ == 704
import Speculate -- go figure...
#else
import Speculate hiding (value)
#endif
import Speculate.Utils.Color
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
      [ black   -| "black"
      , white   -| "white"
      , red     -| "red"
      , green   -| "green"
      , blue    -| "blue"
--    , cyan    -| "cyan"
--    , magenta -| "magenta"
--    , yellow  -| "yellow"
--    , orange  -| "orange"
      , (+)    -:> color -| "+"
      , (-)    -:> color -| "-"
--    , (*)    -:> color -| "*"
--    , negate -:> color -| "negate"
--    , (.+.) -| ".+."
--    , (.-.) -| ".-."
--    , (.*.) -| ".*."
      , chroma     -| "chroma"
      , hue        -| "hue"
      , saturation -| "saturation"
--    , intensity  -| "intensity"
      , value      -| "value"
--    , lightness  -| "lightness"
      , fromHSV    -| "fromHSV"
      , fromHSL    -| "fromHSL"
      , mix        -| "mix"
--    , mixHSV     -| "mixHSV"

      , Just -:> rational -| "Just"

      , s (0 % 1 :: Rational)
      , s (1 % 1 :: Rational)
      , s (1 % 2 :: Rational)

--    , (==) -:> color -| "=="
--    , (/=) -:> color -| "/="
--    , False -| "False"
--    , True  -| "True"
--    , primary -| "primary"
      ]
  } 
