-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.DateZoneL
-- Copyright   :  (c) Martin Perner
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Martin Perner <martin@perner.cc>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A date plugin with localization and location support for Xmobar
--
-- Based on Plugins.DateZone
--
-----------------------------------------------------------------------------

module Plugins.DateZoneL (DateZoneL(..)) where

import Plugins

import Localize

import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.LocalTime.TimeZone.Olson
import Data.Time.LocalTime.TimeZone.Series

data DateZoneL = DateZoneL String String String String Int
    deriving (Read, Show)

instance Exec DateZoneL where
    alias (DateZoneL _ _ a _ _) = a
    start (DateZoneL f l _ z r) cb = do
      setupTimeLocale l
      go
        where go = date f z >>= cb >> tenthSeconds r >> go

date :: String -> String -> IO String
date format zone = do
  timeZone <- getTimeZoneSeriesFromOlsonFile ("/usr/share/zoneinfo/" ++ zone)
  zonedTime <- getZonedTime
  return $ formatTime getTimeLocale format $ utcToLocalTime' timeZone $ zonedTimeToUTC zonedTime
