-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Mem
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A memory monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Mem where

import Plugins.Monitors.Common

memConfig :: IO MConfig
memConfig = mkMConfig
       "Mem: <usedratio>% (<cache>M)" -- template
       ["usedbar", "freebar", "usedratio", "total",  -- available replacements
        "free", "buffer", "cache", "rest", "used"]

fileMEM :: IO String
fileMEM = readFile "/proc/meminfo"

parseMEM :: IO [Float]
parseMEM =
    do file <- fileMEM
       let content = map words $ take 4 $ lines file
           [total, free, buffer, cache] = map (\line -> (read $ line !! 1 :: Float) / 1024) content
           rest = free + buffer + cache
           used = total - rest
           usedratio = used / total
       return [usedratio, total, free, buffer, cache, rest, used]

formatMem :: [Float] -> Monitor [String]
formatMem (r:xs) =
    do let f n = showDigits 0 n
           rr = 100 * r
       ub <- showPercentBar rr r
       fb <- showPercentBar (100 - rr) (1 - r)
       s <- mapM (showWithColors f) (rr:xs)
       return (ub:fb:s)
formatMem _ = return $ replicate 8 "N/A"

runMem :: [String] -> Monitor String
runMem _ =
    do m <- io $ parseMEM
       l <- formatMem m
       parseTemplate l
