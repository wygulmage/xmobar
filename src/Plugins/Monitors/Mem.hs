-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Mem
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A memory monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Mem (memConfig, runMem, totalMem, usedMem) where

import Plugins.Monitors.Common
import qualified Data.Map as M
import System.Console.GetOpt

data MemOpts = MemOpts
  { usedIconPattern :: Maybe IconPattern
  , freeIconPattern :: Maybe IconPattern
  }

defaultOpts :: MemOpts
defaultOpts = MemOpts
  { usedIconPattern = Nothing
  , freeIconPattern = Nothing
  }

options :: [OptDescr (MemOpts -> MemOpts)]
options =
  [ Option "" ["used-icon-pattern"] (ReqArg (\x o ->
     o { usedIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["free-icon-pattern"] (ReqArg (\x o ->
     o { freeIconPattern = Just $ parseIconPattern x }) "") ""
  ]

parseOpts :: [String] -> IO MemOpts
parseOpts argv =
  case getOpt Permute options argv of
    (o, _, []) -> return $ foldr id defaultOpts o
    (_, _, errs) -> ioError . userError $ concat errs

memConfig :: IO MConfig
memConfig = mkMConfig
       "Mem: <usedratio>% (<cache>M)" -- template
       ["usedbar", "usedvbar", "usedipat", "freebar", "freevbar", "freeipat", "usedratio", "freeratio",
        "total", "free", "buffer", "cache", "rest", "used"] -- available replacements

fileMEM :: IO String
fileMEM = readFile "/proc/meminfo"

parseMEM :: IO [Float]
parseMEM =
    do file <- fileMEM
       let content = map words $ take 8 $ lines file
           info = M.fromList $ map (\line -> (head line, (read $ line !! 1 :: Float) / 1024)) content
           [total, free, buffer, cache] = map (info M.!) ["MemTotal:", "MemFree:", "Buffers:", "Cached:"]
           rest = free + buffer + cache
           used = total - M.findWithDefault rest "MemAvailable:" info
           usedratio = used / total
           freeratio = free / total
       return [usedratio, freeratio, total, free, buffer, cache, rest, used, freeratio]

totalMem :: IO Float
totalMem = fmap ((*1024) . (!!1)) parseMEM

usedMem :: IO Float
usedMem = fmap ((*1024) . (!!6)) parseMEM

formatMem :: MemOpts -> [Float] -> Monitor [String]
formatMem opts (r:fr:xs) =
    do let f = showDigits 0
           rr = 100 * r
       ub <- showPercentBar rr r
       uvb <- showVerticalBar rr r
       uipat <- showIconPattern (usedIconPattern opts) r
       fb <- showPercentBar (100 - rr) (1 - r)
       fvb <- showVerticalBar (100 - rr) ( 1 - r)
       fipat <- showIconPattern (freeIconPattern opts) (1 - r)
       rs <- showPercentWithColors r
       fs <- showPercentWithColors fr
       s <- mapM (showWithColors f) xs
       return (ub:uvb:uipat:fb:fvb:fipat:rs:fs:s)
formatMem _ _ = replicate 10 `fmap` getConfigValue naString

runMem :: [String] -> Monitor String
runMem argv =
    do m <- io parseMEM
       opts <- io $ parseOpts argv
       l <- formatMem opts m
       parseTemplate l
