module Test.Speculate
  ( report

  , Args (..)
  , args
  , speculate
  , getArgs

  , foreground
  , background

  , showConstant
  , constant
  , hole
  , ins, eq, ord, eqWith, ordWith, names

  , Expr
  , Instances

  -- useful for declaring Listable instances
  , module Test.LeanCheck

  -- test types & type binding operators:
  , module Test.LeanCheck.Utils

  -- useful export for GHC < 7.10:
  , module Data.Typeable
  )
where

import Data.Typeable
import Test.LeanCheck
import Test.LeanCheck.Utils hiding (comparison)

import Test.Speculate.Expr
  ( Expr
  , constant
  , var
  , hole
  , showConstant
  , Instances
  , ins
  , eq
  , eqWith
  , ord
  , ordWith
  , names
  )
import Test.Speculate.Args
  ( Args (..)
  , args
  , getArgs
  , foreground
  , background
  , processArgs
  , prepareArgs
  , HelpFormat (..)
  , helpText
  , showHelp
  )
import Test.Speculate.Report (report)

speculate :: Args -> IO ()
speculate args = do
  as <- processArgs (prepareArgs args)
  if showHelp as
    then print $ helpText [] HelpFormatDefault (prepareArgs args)
    else report as
