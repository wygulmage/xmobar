{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Utils
-- Copyright: (c) 2010, 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: Jose A Ortega Ruiz <jao@gnu.org>
-- Stability: unstable
-- Portability: unportable
-- Created: Sat Dec 11, 2010 20:55
--
--
-- Miscellaneous utility functions
--
------------------------------------------------------------------------------


module Xmobar.System.Utils (expandHome, changeLoop, hGetLineSafe)
where

import Control.Monad
import Control.Concurrent.STM

import System.Environment
import System.FilePath
import System.IO

#if defined XFT || defined UTF8
import qualified System.IO as S (hGetLine)
#endif

hGetLineSafe :: Handle -> IO String
#if defined XFT || defined UTF8
hGetLineSafe = S.hGetLine
#else
hGetLineSafe = hGetLine
#endif


expandHome :: FilePath -> IO FilePath
expandHome ('~':'/':path) = fmap (</> path) (getEnv "HOME")
expandHome p = return p

changeLoop :: Eq a => STM a -> (a -> IO ()) -> IO ()
changeLoop s f = atomically s >>= go
 where
    go old = do
        f old
        go =<< atomically (do
            new <- s
            guard (new /= old)
            return new)
