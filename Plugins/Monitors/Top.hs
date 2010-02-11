-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Top
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
--  Process activity and memory consumption monitors
--
-----------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Plugins.Monitors.Top ( topCpuConfig
                            , runTopCpu
                            , topMemConfig
                            , runTopMem ) where

import Plugins.Monitors.Common

-- import Control.Monad (zipWithM)
import Control.Exception
import System.Directory
import System.FilePath
import Foreign.C.Types
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

foreign import ccall "unistd.h getpagesize"
  c_getpagesize :: CInt

pageSize :: Int
pageSize = fromIntegral c_getpagesize `div` 1024

processes :: IO [FilePath]
processes =
  fmap (filter isPid) $ getDirectoryContents "/proc"
  where
    isPid (x:_) = x `elem` ['0'..'9']
    isPid _ = False

showInfo :: String -> String -> Float -> Monitor [String]
showInfo nm sms mms = do
  mnw <- getConfigValue maxWidth
  mxw <- getConfigValue minWidth
  let lsms = length sms
      nmw = mnw - lsms - 1
      nmx = mxw - lsms - 1
      rnm = if nmw > 0 then padString nmw nmx " " True nm else nm
  mstr <- showWithColors' sms mms
  both <- showWithColors' (rnm ++ " " ++ sms) mms
  return [nm, mstr, both]

topMemConfig :: IO MConfig
topMemConfig = mkMConfig
               "<both1>"
               [ k ++ n | n <- map show [1::Int .. 5]
                        , k <- ["name", "rss", "both"]]

data Meminfo = MI !String !Int

meminfo :: FilePath -> IO Meminfo
meminfo pidf =
  handle noInfo
         (do s <- readFile $ "/proc" </> pidf </> "stat"
             let fs = words s
                 m = pageSize * read (fs !! 23)
                 n = drop 1 $ init (fs !! 1)
             evaluate $ MI n m)
  where noInfo :: SomeException -> IO Meminfo
        noInfo _ = evaluate $ MI "" 0

meminfos :: IO [Meminfo]
meminfos = do
  fs <- processes
  mapM meminfo fs

sortByMem :: [Meminfo] -> [Meminfo]
sortByMem ps = sortBy (flip (comparing rss)) ps
  where rss (MI _ m) = m

topMemProcesses :: Int -> IO [Meminfo]
topMemProcesses n = fmap (take n . sortByMem) meminfos

showMeminfo :: Meminfo -> Monitor [String]
showMeminfo (MI nm rss) =
  showInfo nm sms (ms / 1024)
    where ms = fromIntegral rss
          sms = showWithUnits 2 1 ms

runTopMem :: [String] -> Monitor String
runTopMem _ = do
  ps <- io $ topMemProcesses 5
  pstr <- mapM showMeminfo ps
  parseTemplate $ concat pstr

topCpuConfig :: IO MConfig
topCpuConfig = mkMConfig
            "<both1>"
            [ k ++ n | n <- map show [1::Int .. 5]
                     , k <- ["name", "cpu", "both"]]

data Timeinfo = TI !String !Float
type Times = IntMap Timeinfo
data TimesVal = TiV !Int !String !Float

cpuTime :: IO Integer
cpuTime = do
  s <- readFile "/proc/stat"
  let ts = map read . tail . words . (!!0) . lines
  return $ sum (ts s)

nullTimesVal :: TimesVal
nullTimesVal = TiV 0 "" 0

timeinfo :: FilePath -> IO TimesVal
timeinfo pidf =
  handle ((\_ -> evaluate nullTimesVal) :: SomeException -> IO TimesVal)
         (do s <- readFile $ "/proc" </> pidf </> "stat"
             let fs = words s
                 pid = read (head fs)
                 rf = read . (fs!!)
                 n = drop 1 $ init (fs!!1)
             evaluate $ TiV pid n (rf 14 + rf 15))

timeinfos :: IO [Times]
timeinfos = do
  fs <- processes
  tis <- mapM timeinfo fs
  return [foldl' acc M.empty tis]
  where acc m (TiV p n t) = if p > 10 then M.insert p (TI n t) m else m

combineTimeInfos :: Times -> Times -> Times
combineTimeInfos t0 t1 = M.intersectionWith timeDiff t1 t0
  where timeDiff (TI n x1) (TI _ x0) = TI n (x1 - x0)

topTimeProcesses :: Int -> IO [Timeinfo]
topTimeProcesses n = do
  c0 <- cpuTime
  (t0:_, t1:_) <- doActionTwiceWithDelay 500000 timeinfos
  c1 <- cpuTime
  let ts = M.elems $ combineTimeInfos t0 t1
      sts = take n $ sortBy (flip (comparing tm)) ts
      tm (TI _ t) = t
      lapse = fromIntegral (c1 - c0) / 100
      norm (TI nm t) = TI nm (t/lapse)
  return $ if lapse > 0 then map norm sts else sts

showTimeInfo :: Timeinfo -> Monitor [String]
showTimeInfo (TI n t) = showInfo n (showDigits 2 t) t

runTopCpu :: [String] -> Monitor String
runTopCpu _ = do
  ps <- io $ topTimeProcesses 5
  pstr <- mapM showTimeInfo ps
  parseTemplate $ concat pstr
