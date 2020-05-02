{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Utils
-- Copyright: (c) 2010, 2018, 2020 Jose Antonio Ortega Ruiz
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


module Xmobar.System.Utils (expandHome, changeLoop, onSomeException)
where

import Control.Monad
import Control.Concurrent.STM

import System.Environment
import System.FilePath
import Control.Exception

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

-- | Like 'finally', but only performs the final action if there was an
-- exception raised by the computation.
--
-- Note that this implementation is a slight modification of
-- onException function.
onSomeException :: IO a -> (SomeException -> IO b) -> IO a
onSomeException io what = io `catch` \e -> do _ <- what e
                                              throwIO (e :: SomeException)

