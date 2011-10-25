-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.DateL
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Martin Perner <martin@perner.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A date plugin with localization for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.DateL (DateL(..)) where

import Plugins
import Localize

import System.Time

data DateL = DateL String String String Int
    deriving (Read, Show)

instance Exec DateL where
    alias (DateL _ _ a _) = a
    start (DateL f l _ r) cb = do
      setupTimeLocale l
      go
        where go = date f >>= cb >> tenthSeconds r >> go

date :: String -> IO String
date format = do
  t <- toCalendarTime =<< getClockTime
  return $ formatCalendarTime getTimeLocale format t
