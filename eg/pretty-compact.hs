{-# LANGUAGE TypeSynonymInstances, TemplateHaskell #-}
import Test.Speculate hiding (string, char)
import Test.LeanCheck
import Data.Function (on)
import Data.List (isPrefixOf)

import Text.PrettyPrint.Compact

renderWidth :: Monoid a => Int -> Doc a -> String
renderWidth n = renderWith defaultOptions{optsPageWidth = n}

renders :: Monoid a => Doc a -> [String]
renders d = [renderWidth n d | n <- [1..8] ++ [10,20..80]]

instance Monoid a => Eq (Doc a) where
  (==) = (==) `on` renders

instance Monoid a => Ord (Doc a) where
  (<=) = and .: (zipWith isPrefixOf `on` renders)
    where (.:) = (.) . (.)

instance Monoid a => Listable (Doc a) where
  tiers = cons1 text
       \/ cons2 (<>)
       \/ cons2 (<+>)
       \/ cons2 ($$)
       \/ cons2 (</>)
       \/ cons2 (<//>)
       \/ cons2 (<$$>)

instance Monoid a => Show (Doc a) where
  show = render

da :: Doc Any
da = undefined

instance Name (Doc a) where name _ = "d1"

main :: IO ()
main = speculate args
  { instances = [reifyInstances da]
  , showConditions = False
  , maxSize = 4
  , maxTests = 360
  , constants =
      [ constant "text"   $ text   ->: da
      , constant "char"   $ char   ->: da
      , constant "flush"  $ flush  -:> da
      , constant "hang"   $ hang   ->:> da
      , constant "<>"     $ (<>)   -:> da
      , constant "<+>"    $ (<+>)  -:> da
      , constant "$$"     $ ($$)   -:> da
      , constant "</>"    $ (</>)  -:> da
      , constant "<//>"   $ (<//>) -:> da
      , constant "<$$>"   $ (<$$>) -:> da
      ]
  }
