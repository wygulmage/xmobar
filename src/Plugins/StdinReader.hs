-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.StdinReader
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading from stdin
--
-----------------------------------------------------------------------------

module Plugins.StdinReader where

import Prelude
import System.Posix.Process
import System.Exit
import System.IO
import Control.Exception (SomeException(..), handle)
import Plugins

data StdinReader = StdinReader
    deriving (Read, Show)

instance Exec StdinReader where
    start StdinReader cb = do
        cb =<< handle (\(SomeException e) -> do hPrint stderr e; return "")
                      (hGetLineSafe stdin)
        eof <- hIsEOF stdin
        if eof
            then exitImmediately ExitSuccess
            else start StdinReader cb
