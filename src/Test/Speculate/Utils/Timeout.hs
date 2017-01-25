module Test.Speculate.Utils.Timeout
  ( timeoutToNothing
  , fromTimeout
  , timeoutToFalse
  , timeoutToTrue
  )
where

import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (evaluate)
import System.Timeout
import Data.Maybe (fromMaybe)

-- TODO: Move this into LeanCheck?

-- | In microseconds
usTimeoutToNothing :: Int -> a -> Maybe a
usTimeoutToNothing n x = unsafePerformIO . timeout n $ do
  evaluate x >>= return

-- | Returns Nothing if value cannot be evaluated to WHNF in a given number of seconds
timeoutToNothing :: RealFrac s => s -> a -> Maybe a
timeoutToNothing n = usTimeoutToNothing $ round (n * 1000000)

fromTimeout :: RealFrac s => s -> a -> a -> a
fromTimeout n x = fromMaybe x . timeoutToNothing n

timeoutToFalse :: RealFrac s => s -> Bool -> Bool
timeoutToFalse n = fromTimeout n False

timeoutToTrue :: RealFrac s => s -> Bool -> Bool
timeoutToTrue n = fromTimeout n True
