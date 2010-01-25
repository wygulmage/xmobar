-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Disk
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A disk usage monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Disk (diskConfig, runDisk) where

import qualified Data.ByteString.Lazy.Char8 as B
import Plugins.Monitors.Common
import Data.List (isPrefixOf, find, intercalate)
import Numeric (floatToDigits)

diskConfig :: IO MConfig
diskConfig = mkMConfig "" ["total", "read", "write"]

type DevName = String
type Path = String

mountedDevices :: IO [(DevName, Path)]
mountedDevices = do
  s <- B.readFile "/etc/mtab"
  return (parseMountedDevices s)

parseMountedDevices :: B.ByteString -> [(DevName, Path)]
parseMountedDevices =
  map undev . filter isDev . map (firstTwo . B.words) . B.lines
    where
      firstTwo (a:b:_) = (B.unpack a, B.unpack b)
      firstTwo _ = ("", "")
      isDev (d, _) = "/dev/" `isPrefixOf` d
      undev (d, f) = (drop 5 d, f)

diskData :: IO [(DevName, [Float])]
diskData = do
  s <- B.readFile "/proc/diskstats"
  let extract ws = (head ws, map read (tail ws))
  return $ map (extract . map B.unpack . drop 2 . B.words) (B.lines s)

mountedData :: [String] -> IO [(DevName, Path, [Float])]
mountedData xs = do
  devs <- mountedDevices
  (dt, dt') <- doActionTwiceWithDelay 950000 diskData
  return $ parseData xs devs dt dt'

parseData :: [String] -> [(DevName, Path)]
             -> [(DevName, [Float])] -> [(DevName, [Float])]
             -> [(DevName, Path, [Float])]
parseData reqs mounted dat dat2 =
  let rm = filter isRequested mounted
      isRequested (dev, path) = dev `elem` reqs || path `elem` reqs
      findDat d = find ((==d) .fst)
      format (dev, path) =
        let f1 = findDat dev dat
            f2 = findDat dev dat2
        in
         case (f1, f2) of
          (Just (_, x), Just (_, y)) -> formatDev path (dev, zipWith (-) y x)
          _ -> (dev, path, [0, 0, 0])
  in map format rm

formatDev :: Path -> (DevName, [Float]) -> (DevName, Path, [Float])
formatDev path (dev, xs) =
  let rSp = speed (xs !! 2) (xs !! 3)
      wSp = speed (xs !! 6) (xs !! 7)
      sp =  speed (xs !! 2 + xs !! 6) (xs !! 3 + xs !! 7)
      speed x t = if t == 0 then 0 else 500 * x / t
      dat = if length xs > 6 then [sp, rSp, wSp] else [0, 0, 0]
  in (dev, path, dat)

speedToStr :: Int -> Float -> String
speedToStr n x
  | n > 2 || x < 103 = show (rInt x) ++ units !! n
  | x < 1024 = "0." ++ s2 ds ++ units !! (n + 1)
  | otherwise = speedToStr (n + 1) (x / 1024)
  where units = ["B", "K", "M", "T"]
        rInt = round :: Float ->Integer
        s2 (a:b:_) = show a ++ show b
        s2 as = show (head as) ++ "0"
        (ds, _) = floatToDigits 10 (x / 1024)

runDisk' :: String -> [Float] -> Monitor String
runDisk' tmp xs = do
  setConfigValue tmp template
  s <- mapM (showWithColors (speedToStr 1)) xs
  parseTemplate s

findTempl :: DevName -> Path -> [(String, String)] -> String
findTempl dev path disks =
  case find devOrPath disks of
    Just (_, t) -> t
    Nothing -> ""
  where devOrPath (d, _) = d == dev || d == path

runDisk :: [(String, String)] -> [String] -> Monitor String
runDisk disks _ = do
  dat <- io $ mountedData (map fst disks)
  strs <- mapM (\(d, p, xs) -> runDisk' (findTempl d p disks) xs) dat
  return $ intercalate " " strs
