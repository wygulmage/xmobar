-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Cpu
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A cpu monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Cpu where

import Plugins.Monitors.Common
import qualified Data.ByteString.Lazy.Char8 as B

cpuConfig :: IO MConfig
cpuConfig = mkMConfig
       "Cpu: <total>%"
       ["bar","total","user","nice","system","idle"]

cpuData :: IO [Float]
cpuData = do s <- B.readFile "/proc/stat"
             return $ cpuParser s

cpuParser :: B.ByteString -> [Float]
cpuParser =
    map (read . B.unpack) . tail . B.words . head . B.lines

parseCPU :: IO [Float]
parseCPU =
    do (a,b) <- doActionTwiceWithDelay 750000 cpuData
       let dif = zipWith (-) b a
           tot = foldr (+) 0 dif
           percent = map (/ tot) dif
       return percent

formatCpu :: [Float] -> Monitor [String]
formatCpu [] = return $ replicate 6 ""
formatCpu xs = do
  let t = foldr (+) 0 $ take 3 xs
  b <- showPercentBar (100 * t) t
  ps <- showPercentsWithColors (t:xs)
  return (b:ps)

runCpu :: [String] -> Monitor String
runCpu _ =
    do c <- io parseCPU
       l <- formatCpu c
       parseTemplate l
