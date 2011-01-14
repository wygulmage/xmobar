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
import Data.List (isPrefixOf, transpose, unfoldr)

multiCpuConfig :: IO MConfig
multiCpuConfig =
  mkMConfig "Cpu: <total>%" $
            ["auto" ++ k | k <- monitors] ++
            [ k ++ n     | n <- "" : map show [0 :: Int ..]
                         , k <- monitors]
    where monitors = ["bar","total","user","nice","system","idle"]


cpuData :: IO [[Float]]
cpuData = parse `fmap` B.readFile "/proc/stat"
  where parse = map parseList . cpuLists
        cpuLists = takeWhile isCpu . map B.words . B.lines
        isCpu (w:_) = "cpu" `isPrefixOf` B.unpack w
        isCpu _ = False
        parseList = map (parseFloat . B.unpack) . tail

parseCpuData :: IO [[Float]]
parseCpuData =
  do (as, bs) <- doActionTwiceWithDelay 950000 cpuData
     let p0 = zipWith percent bs as
     return p0

percent :: [Float] -> [Float] -> [Float]
percent b a = if tot > 0 then map (/ tot) $ take 4 dif else [0, 0, 0, 0]
  where dif = zipWith (-) b a
        tot = foldr (+) 0 dif

formatMultiCpus :: [[Float]] -> Monitor [String]
formatMultiCpus [] = return []
formatMultiCpus xs = fmap concat $ mapM formatCpu xs

formatCpu :: [Float] -> Monitor [String]
formatCpu xs
  | length xs < 4 = showPercentsWithColors $ replicate 6 0.0
  | otherwise = let t = foldr (+) 0 $ take 3 xs
                in do b <- showPercentBar (100 * t) t
                      ps <- showPercentsWithColors (t:xs)
                      return (b:ps)

splitEvery :: (Eq a) => Int -> [a] -> [[a]]
splitEvery n = unfoldr (\x -> if null x then Nothing else Just $ splitAt n x)

groupData :: [String] -> [[String]]
groupData = transpose . tail . splitEvery 6

formatAutoCpus :: [String] -> Monitor [String]
formatAutoCpus [] = return $ replicate 6 ""
formatAutoCpus xs = return $ map unwords (groupData xs)

runMultiCpu :: [String] -> Monitor String
runMultiCpu _ =
  do c <- io parseCpuData
     l <- formatMultiCpus c
     a <- formatAutoCpus l
     parseTemplate $ a ++ l