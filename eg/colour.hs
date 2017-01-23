{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-} -- for GHC < 7.10
#if __GLASGOW_HASKELL__ == 704
import Test.Speculate -- go figure...
#else
import Test.Speculate hiding (value)
#endif
import Test.Speculate.Utils.Colour
import Test.LeanCheck
import Data.Ratio

deriving instance Typeable Colour -- for GHC < 7.10

-- Just for Listable.tiers enumeration
data ColourComponent = ColourComponent Rational

instance Listable ColourComponent where
  tiers = mapT (ColourComponent . uncurry (%))
        $ tiers `suchThat` (\(n,d) -> n >= 0 && d > 0 && n <= d && n `gcd` d == 1)
                `ofWeight` 0

instance Listable Colour where
  tiers = cons3 (\(ColourComponent r) (ColourComponent g) (ColourComponent b) -> RGB r g b)

colour :: Colour
colour = undefined

rational :: Rational
rational = undefined

main :: IO ()
main = speculate args
  { customTypeInfo =
      [ typeInfo colour    "c"
      ]
  , maxSize = 4
  , constants =
      [ constant "+"       $ (+)    -:> colour
      , constant "-"       $ (-)    -:> colour
--    , constant "*"       $ (*)    -:> colour
--    , constant "negate"  $ negate -:> colour
--    , constant ".+."     $ (.+.)  -:> colour
--    , constant ".-."     $ (.-.)  -:> colour
--    , constant ".*."     $ (.*.)  -:> colour
      , constant "chroma"     chroma
      , constant "hue"        hue
      , constant "saturation" saturation
--    , constant "intensity"  intensity
      , constant "value"      value
      , constant "lightness"  lightness
      , constant "fromHSV"    fromHSV
      , constant "fromHSL"    fromHSL
      , constant "mix"        mix
--    , constant "mixHSV"     mixHSV
      ]
    , backgroundConstants =
      [ constant "black"     black
      , constant "white"     white
      , constant "red"       red
      , constant "grey"      grey
      , constant "green"     green
      , constant "blue"      blue
--    , constant "cyan"      cyan
--    , constant "magenta"   magenta
--    , constant "yellow"    yellow
--    , constant "orange"    orange
      , constant "Just"     $ Just -:> rational
      , constant "Nothing"  (Nothing :: Maybe Rational)

      , showConstant (0 % 1 :: Rational)
      , showConstant (1 % 1 :: Rational)
      , showConstant (1 % 2 :: Rational)

--    , constant "<=" $ (<=) -:> rational
--    , constant "<"  $ (<)  -:> rational

--    , constant "==" $ (==) -:> colour
--    , constant "/=" $ (/=) -:> colour
--    , constant "primary" primary
      ]
  } 
