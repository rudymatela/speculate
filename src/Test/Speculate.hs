module Test.Speculate
  ( report

  , Args (..)
  , args
  , speculate
  , getArgs

  , foreground
  , background

  , module Test.Speculate.Expr
  , module Test.LeanCheck.Utils
  , module Test.LeanCheck

  -- useful export for GHC < 7.10:
  , module Data.Typeable
  )
where

import Data.Typeable
import Data.Dynamic
import Data.Maybe
import Data.List hiding (insert)
import qualified Data.List as L
import Data.Function
import Control.Monad
import Test.LeanCheck
import Test.LeanCheck.Utils hiding (comparison)
import Test.LeanCheck.Tiers (unorderedPairsWith)
import Test.Speculate.Utils
import Test.Speculate.Utils.Colour
import Data.Ratio ((%))
import Data.Monoid ((<>))

import Test.Speculate.Expr
import Test.Speculate.Reason
import Test.Speculate.CondReason
import Test.Speculate.SemiReason
import Test.Speculate.Engine
import Test.Speculate.Sanity
import Test.Speculate.Args
import Test.Speculate.Report

speculate :: Args -> IO ()
speculate args = do
  as <- processArgs (prepareArgs args)
  if showHelp as
    then print $ helpText [] HelpFormatDefault (prepareArgs args)
    else report as
