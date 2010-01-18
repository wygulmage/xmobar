-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.MultiCpu
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A multi-cpu monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.MultiCpu(multiCpuConfig, runMultiCpu) where

import Plugins.Monitors.Common
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List(isPrefixOf)

multiCpuConfig :: IO MConfig
multiCpuConfig = mkMConfig
                 "Cpu: <total>"
                 [ k ++ n | n <- "":(map show [0 :: Int ..])
                          , k <- ["total","user","nice","system","idle"]]


cpuData :: IO [[Float]]
cpuData = do s <- B.readFile "/proc/stat"
             return $ cpuParser s

cpuParser :: B.ByteString -> [[Float]]
cpuParser = map parseList . cpuLists
  where cpuLists = takeWhile isCpu . map B.words . B.lines
        isCpu (w:_) = "cpu" `isPrefixOf` (B.unpack w)
        isCpu _ = False
        parseList = map (read . B.unpack) . tail

parseCpuData :: IO [[Float]]
parseCpuData =
  do (as, bs) <- doActionTwiceWithDelay 350000 cpuData
     let p0 = zipWith percent bs as
     (as', bs') <- doActionTwiceWithDelay 350000 cpuData
     let p1 = zipWith percent bs' as'
     return $ zipWith (\x y -> zipWith (\a b -> (a + b) / 2.0) x y)  p1 p0

percent :: [Float] -> [Float] -> [Float]
percent b a = if tot > 0 then map (/ tot) $ take 4 dif else [0, 0, 0, 0]
  where dif = zipWith (-) b a
        tot = foldr (+) 0 dif

formatMultiCpus :: [[Float]] -> Monitor [String]
formatMultiCpus [] = return $ take 15 (repeat "0%")
formatMultiCpus xs = fmap concat $ mapM formatCpu xs

formatCpu :: [Float] -> Monitor [String]
formatCpu x
  | length x < 4 = return $ take 5 (repeat "")
  | otherwise  = mapM (showWithColors f) . map (* 100) $ (t:x)
            where f s = floatToPercent (s / 100)
                  t = foldr (+) 0 $ take 3 x

runMultiCpu :: [String] -> Monitor String
runMultiCpu _ =
  do c <- io $ parseCpuData
     l <- formatMultiCpus c
     parseTemplate l
