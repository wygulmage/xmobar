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

module Plugins.Monitors.MultiCpu where

import Plugins.Monitors.Common
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List(isPrefixOf)

multiCpuConfig :: IO MConfig
multiCpuConfig = mkMConfig
                 "Cpu: <total>"
                 [ k ++ n | n <- "":(map show [0 :: Int ..])
                          , k <- ["total","user","nice","system","idle"]]


multiCpuData :: IO [[Float]]
multiCpuData = do s <- B.readFile "/proc/stat"
                  return $ multiCpuParser s

multiCpuParser :: B.ByteString -> [[Float]]
multiCpuParser = map (map read . tail) . lns
  where lns = takeWhile isCpu . map unpW . B.lines
        isCpu (w:_) = "cpu" `isPrefixOf` w
        isCpu _ = False
        unpW = map B.unpack . B.words

parseMultiCpu :: IO [[Float]]
parseMultiCpu =
  do (as, bs) <- doActionTwiceWithDelay 350000 multiCpuData
     let p0 = zipWith percent bs as
     (as', bs') <- doActionTwiceWithDelay 350000 multiCpuData
     let p1 = zipWith percent bs' as'
     return $ zipWith (\x y -> zipWith (\a b -> (a + b) / 2.0) x y)  p1 p0

percent :: [Float] -> [Float] -> [Float]
percent b a = if tot > 0 then map (/ tot) $ take 4 dif else [0, 0, 0, 0]
  where dif = zipWith (-) b a
        tot = foldr (+) 0 dif

formatMultiCpus :: [[Float]] -> Monitor [String]
formatMultiCpus [] = return $ take 15 (repeat "0%")
formatMultiCpus xs = fmap concat $ mapM formatMultiCpu xs

formatMultiCpu :: [Float] -> Monitor [String]
formatMultiCpu x
  | length x < 4 = return $ take 5 (repeat "")
  | otherwise  = mapM (showWithColors f) . map (* 100) $ (t:x)
            where f s = floatToPercent (s / 100)
                  t = foldr (+) 0 $ take 3 x

runMultiCpu :: [String] -> Monitor String
runMultiCpu _ =
  do c <- io $ parseMultiCpu
     l <- formatMultiCpus c
     parseTemplate l
