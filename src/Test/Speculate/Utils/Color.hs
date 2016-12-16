-- | Simple color module.  It is used:
--   * to colorize graphs in partial order graph generation ("Speculate");
--   * as an example on eg folder.
module Test.Speculate.Utils.Color
  ( Color (RGB)
  , showRGB
  , (.+.), (.-.), (.*.)
  , black, white
  , red, green, blue
  , cyan, magenta, yellow
  , orange
  , rgb, cmy
  , chroma
  , hue
  , intensity, value, lightness
  , saturation, saturationHSV, saturationHSL, saturationHSI
  , fromHSV, fromHSL
  , mix, mixHSV
  , primary

  -- * Misc Utils
  , frac
  , coerceRatio
  )
where

import Data.Char
import Data.List
import Data.Maybe
import Data.Ratio
import Data.Tuple
import Data.Functor ((<$>)) -- for GHC < 7.10
import Control.Applicative ((<*>)) -- for GHC < 7.10

data Color = RGB Rational Rational Rational
  deriving (Eq, Ord)

instance Show Color where
  show c@(RGB r g b) = "RGB (" ++ show r ++ ") (" ++ show g ++ ") (" ++ show b ++ ")"
          ++ " {- " ++ showRGB c ++ " -}"

showRGB :: Color -> String
showRGB (RGB r g b) = "#" ++ hexRatio r ++ hexRatio g ++ hexRatio b

hexRatio :: Integral a => Ratio a -> String
hexRatio r = hex $ numerator r * 0xFF `div` denominator r

hex :: Integral a => a -> String
hex = (\s -> case s of
               []  -> "00"
               [c] -> '0':[c]
               cs  -> cs)
    . map (intToDigit . coerceNum)
    . reverse
    . unfoldr (\n -> listToMaybe [swap $ n `divMod` 16 | n /= 0])

coerceNum :: (Integral a, Num b) => a -> b
coerceNum = fromInteger . toInteger

coerceRatio :: (Integral a, Integral b) => Ratio a -> Ratio b
coerceRatio r = coerceNum (numerator r) % coerceNum (denominator r)

mod1 :: Integral a => Ratio a -> Ratio a
mod1 r = (numerator r `mod` denominator r) % denominator r

modulo :: Integral a => Ratio a -> Ratio a -> Ratio a
n `modulo` d = mod1 (n / d) * d

frac :: Integral a => Ratio a -> Ratio a
frac r | r < 0 = 0
       | r > 1 = 1
       | otherwise = r

instance Num Color where
  RGB r1 g1 b1 + RGB r2 g2 b2 = RGB (frac $ r1 + r2) (frac $ g1 + g2) (frac $ b1 + b2)
  RGB r1 g1 b1 - RGB r2 g2 b2 = RGB (frac $ r1 - r2) (frac $ g1 - g2) (frac $ b1 - b2)
  RGB r1 g1 b1 * RGB r2 g2 b2 = RGB        (r1 * r2)        (g1 * g2)        (b1 * b2)
  negate (RGB r g b) = RGB (1 - r) (1 - g) (1 - b)
  abs c = c
  signum c = 1
  fromInteger i = let j = i `div` 0x100
                      k = j `div` 0x100
                  in RGB (k `mod` 0x100 % 255) (j `mod` 0x100 % 255) (i `mod` 0x100 % 255)

(.+.) :: Color -> Color -> Color
c1 .+. c2 = negate $ negate c1 + negate c2

(.-.) :: Color -> Color -> Color
c1 .-. c2 = negate $ negate c1 - negate c2

(.*.) :: Color -> Color -> Color
c1 .*. c2 = negate $ negate c1 * negate c2

black :: Color
black = RGB 0 0 0

white :: Color
white = RGB 1 1 1

red :: Color
red = RGB 1 0 0

green :: Color
green = RGB 0 1 0

blue :: Color
blue = RGB 0 0 1

cyan :: Color
cyan = RGB 0 1 1

magenta :: Color
magenta = RGB 1 0 1

yellow :: Color
yellow = RGB 1 1 0

orange :: Color
orange = RGB 1 (1%2) 0

rgb :: Color -> (Rational, Rational, Rational)
rgb (RGB r g b) = (r,g,b)

cmy :: Color -> (Rational, Rational, Rational)
cmy (RGB r g b) = (1 - r, 1 - g, 1 - b)

maxi :: Color -> Rational
maxi (RGB r g b) = maximum [r,g,b]

mini :: Color -> Rational
mini (RGB r g b) = minimum [r,g,b]

chroma :: Color -> Rational
chroma c = maxi c - mini c

hue :: Color -> Maybe Rational
hue color@(RGB r g b) = (\h' -> mod1 $ h' / 6) <$> h' -- * 60 / 360
  where
  c = chroma color
  m = maxi color
  h' | c == 0 = Nothing
     | m == r = Just $ (g - b) / c
     | m == g = Just $ (b - r) / c + 2
     | m == b = Just $ (r - g) / c + 4

intensity :: Color -> Rational
intensity (RGB r g b) = (r + g + b) / 3

value :: Color -> Rational
value c = maxi c

lightness :: Color -> Rational
lightness c = (maxi c + mini c) / 2

saturation :: Color -> Rational
saturation = saturationHSV

saturationHSV :: Color -> Rational
saturationHSV c =
  if value c == 0
    then 0
    else chroma c / value c

saturationHSL :: Color -> Rational
saturationHSL c =
  if lightness c == 1
    then 0
    else chroma c / (1 - abs (2 * lightness c - 1))

saturationHSI :: Color -> Rational
saturationHSI c =
  case intensity c of
    0 -> 0
    i -> 1 - mini c/i

fromHSV :: Maybe Rational -> Rational -> Rational -> Color
fromHSV h s v = RGB (r' + m) (g' + m) (b' + m)
  where
  h' = case h of
         Nothing -> 0
         Just h' -> mod1 h'
  m = v - c
  (r',g',b')
    | 0%6 <= h' && h' <= 1%6 = (c,x,0)
    | 1%6 <= h' && h' <= 2%6 = (x,c,0)
    | 2%6 <= h' && h' <= 3%6 = (0,c,x)
    | 3%6 <= h' && h' <= 4%6 = (0,x,c)
    | 4%6 <= h' && h' <= 5%6 = (x,0,c)
    | 5%6 <= h' && h' <= 6%6 = (c,0,x)
  x = c * (1 - mod1 ((h'*6) `modulo` 2 - 1))
  c = v * s

fromHSL :: Maybe Rational -> Rational -> Rational -> Color
fromHSL h s l = RGB (r' + m) (g' + m) (b' + m)
  where
  h' = case h of
         Nothing -> 0
         Just h' -> mod1 h'
  x = c * (1 - abs ((h'*6) `modulo` 2 - 1))
  (r',g',b')
    | 0%6 <= h' && h' <= 1%6 = (c,x,0)
    | 1%6 <= h' && h' <= 2%6 = (x,c,0)
    | 2%6 <= h' && h' <= 3%6 = (0,c,x)
    | 3%6 <= h' && h' <= 4%6 = (0,x,c)
    | 4%6 <= h' && h' <= 5%6 = (x,0,c)
    | 5%6 <= h' && h' <= 6%6 = (c,0,x)
  m = l - c / 2
  c = (1 - abs (2*l - 1)) * s

mix :: Color -> Color -> Color
mix (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB ((r1 + r2) / 2) ((g1 + g2) / 2) ((b1 + b2) / 2)

mixHSV :: Color -> Color -> Color
mixHSV c1 c2 = fromHSV (mod1 <$> ((+) <$> hue c1 <*> hue c2))
                       ((saturationHSV c1 + saturationHSV c2) / 2)
                       ((value c1 + value c2) / 2)

primary :: Color -> Bool
primary c = c == red
         || c == green
         || c == blue
