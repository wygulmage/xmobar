------------------------------------------------------------------------------
-- |
-- Module      : Plugins.Uptime
-- Copyright   : (c) 2010 Jose Antonio Ortega Ruiz
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : jao@gnu.org
-- Stability   : unstable
-- Portability : unportable
-- Created: Sun Dec 12, 2010 20:26
--
--
-- Uptime
--
------------------------------------------------------------------------------


module Plugins.Uptime (Uptime(..)) where

import Plugins

import qualified Data.ByteString.Lazy.Char8 as B

data Uptime = Uptime String Int
    deriving (Read, Show)

instance Exec Uptime where
    alias (Uptime a _) = a
    run   (Uptime _ _) = uptime
    rate  (Uptime _ r) = r

readUptime :: IO Float
readUptime =
  fmap (read . B.unpack . head . B.words) (B.readFile "/proc/uptime")

secsPerDay :: Integer
secsPerDay = 24 * 3600

uptime :: IO String
uptime = do
  t <- readUptime
  let tsecs = floor t
      secs = tsecs `mod` secsPerDay
      days = tsecs `quot` secsPerDay
      hrs = secs `quot` 3600
      mins = (secs `mod` 3600) `div` 60
      dstr | days == 0 = ""
           | otherwise = show days ++ "d "
      str x | x < 10 = '0':show x
            | otherwise = show x
  return $ dstr ++ str hrs ++ ":" ++ str mins
