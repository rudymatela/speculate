-- |
-- Module      : Test.Speculate
-- Copyright   : (c) 2016-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- __ Speculate: discovery of properties by reasoning from test results __
--
-- Speculate automatically discovers laws about Haskell functions.
-- Those laws involve:
--
-- * equations,             such as @ id x == x @;
-- * inequalities,          such as @ 0 <= x * x @;
-- * conditional equations, such as @ x \<= 0  ==\>  x + abs x == 0 @.
--
-- _Example:_ the following program prints laws about @0@, @1@, @+@ and @abs@.
--
-- > import Test.Speculate
-- >
-- > main :: IO ()
-- > main = speculate args
-- >   { constants =
-- >       [ showConstant (0::Int)
-- >       , showConstant (1::Int)
-- >       , constant "+"   ((+)  :: Int -> Int -> Int)
-- >       , constant "abs" (abs  :: Int -> Int)
-- >       , background
-- >       , constant "<="  ((<=) :: Int -> Int -> Bool)
-- >       ]
-- >   }
module Test.Speculate
  ( speculate
  , Args (..)
  , args

  -- * The constants list
  -- | The following combinators are used to build
  --   the 'constants' list from 'Args'.
  , Expr
  , constant
  , showConstant
  , hole
  , foreground
  , background

  -- * The instances list
  -- | The following combinators are used to build
  --   the 'instances' list from 'Args'.
  , Instances
  , ins
  , reifyEq
  , reifyOrd
  , reifyEqOrd
  , reifyListable
  , mkEq
  , mkOrd
  , mkOrdLessEqual
  , mkListable
  , mkNameWith
  , names

  -- * Misc.
  , report
  , getArgs

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
  , reifyEq
  , reifyOrd
  , reifyEqOrd
  , reifyListable
  , mkEq
  , mkOrd
  , mkOrdLessEqual
  , mkListable
  , mkNameWith
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

-- | Calls Speculate.  See the example above (at the top of the file).
-- Its only argument is an 'Args' structure.
speculate :: Args -> IO ()
speculate args = do
  as <- processArgs (prepareArgs args)
  if showHelp as
    then print $ helpText [] HelpFormatDefault (prepareArgs args)
    else report as
