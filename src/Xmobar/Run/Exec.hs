-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Exec
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The 'Exec' class and the 'Command' data type.
--
-- The 'Exec' class rappresents the executable types, whose constructors may
-- appear in the 'Config.commands' field of the 'Config.Config' data type.
--
-- The 'Command' data type is for OS commands to be run by xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Run.Exec (Exec (..), tenthSeconds) where

import Prelude
import Data.Char
import Control.Concurrent

import Xmobar.System.Signal

-- | Work around to the Int max bound: since threadDelay takes an Int, it
-- is not possible to set a thread delay grater than about 45 minutes.
-- With a little recursion we solve the problem.
tenthSeconds :: Int -> IO ()
tenthSeconds s | s >= x = do threadDelay (x * 100000)
                             tenthSeconds (s - x)
               | otherwise = threadDelay (s * 100000)
               where x = (maxBound :: Int) `div` 100000

class Show e => Exec e where
    alias   :: e -> String
    alias   e    = takeWhile (not . isSpace) $ show e
    rate    :: e -> Int
    rate    _    = 10
    run     :: e -> IO String
    run     _    = return ""
    start   :: e -> (String -> IO ()) -> IO ()
    start   e cb = go
        where go = run e >>= cb >> tenthSeconds (rate e) >> go
    trigger :: e -> (Maybe SignalType -> IO ()) -> IO ()
    trigger _ sh  = sh Nothing
