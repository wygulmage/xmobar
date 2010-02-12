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

{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}

module Plugins.Monitors.Top (startTopCpu, topMemConfig, runTopMem) where

import Plugins.Monitors.Common

import Control.Exception (SomeException, handle, evaluate)
import System.Directory
import System.FilePath
import System.Posix.Unistd (getSysVar, SysVar(ClockTick))
import Foreign.C.Types
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import Data.IORef

import Data.IntMap (IntMap)
import qualified Data.IntMap as M


topMemConfig :: IO MConfig
topMemConfig = mkMConfig "<both1>" [ k ++ n | n <- map show [1..maxProc]
                                            , k <- ["name", "rss", "both"]]

topCpuConfig :: IO MConfig
topCpuConfig = mkMConfig "<both1>" [ k ++ n | n <- map show [1..maxProc]
                                            , k <- ["name", "cpu", "both"]]


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

defHandle :: a -> SomeException -> IO a
defHandle def _ = evaluate def

handlePidFile :: a -> ([String] -> IO a) -> FilePath -> IO a
handlePidFile def action pidf =
  handle (defHandle def) (fmap words (readFile f) >>= action)
  where f = "/proc" </> pidf </> "stat"

maxProc :: Int
maxProc = 5

type Meminfo = (String, Int)

meminfo :: FilePath -> IO Meminfo
meminfo = handlePidFile ("", 0) $ \fs ->
  let !m = pageSize * read (fs !! 23)
      !n = drop 1 $ init (fs !! 1)
  in evaluate (n, m)

meminfos :: IO [Meminfo]
meminfos = do
  fs <- processes
  mapM meminfo fs

topMemProcesses :: Int -> IO [Meminfo]
topMemProcesses n = fmap (take n . sbm) meminfos
  where sbm = sortBy (flip (comparing snd))

showMeminfo :: Meminfo -> Monitor [String]
showMeminfo (nm, rss) =
  showInfo nm sms (ms / 1024)
    where ms = fromIntegral rss
          sms = showWithUnits 2 1 ms

runTopMem :: [String] -> Monitor String
runTopMem _ = do
  ps <- io $ topMemProcesses maxProc
  pstr <- mapM showMeminfo ps
  parseTemplate $ concat pstr

type Pid = Int
type TimeInfo = (String, Float)
type TimeEntry = (Pid, TimeInfo)
type Times = IntMap TimeInfo
type TimesRef = IORef Times

timeinfo :: FilePath -> IO TimeEntry
timeinfo = handlePidFile (0, ("", 0)) $ \fs ->
  let rf = read . (fs!!)
      !pid = read (head fs)
      !n = drop 1 $ init (fs!!1)
      !t = rf 13 + rf 14
  in evaluate $ (pid, (n, t))

timeinfos :: IO Times
timeinfos = do
  fs <- processes
  tis <- mapM timeinfo $! fs
  return $ foldl' acc M.empty tis
  where acc m (p, (n, t)) = M.insert p (n, t) m

combineTimeInfos :: Times -> Times -> Times
combineTimeInfos !t0 !t1 = M.intersectionWith timeDiff t1 t0
  where timeDiff (n, x1) (_, x0) = (n, (x1 - x0))

topTimeProcesses :: Int -> TimesRef -> Float -> IO [TimeInfo]
topTimeProcesses n tref lapse = do
  t1 <- timeinfos
  t0 <- readIORef tref
  writeIORef tref $! t1
  let !ts = M.elems $ combineTimeInfos t0 t1
      !sts = take n $ sortBy (flip (comparing snd)) ts
      !nts = map norm sts
      norm (nm, t) = (nm, 100 * t / lapse)
  return nts

showTimeInfo :: TimeInfo -> Monitor [String]
showTimeInfo (n, t) = showInfo n (showDigits 1 t) t

runTopCpu :: TimesRef -> Float -> [String] -> Monitor String
runTopCpu tref lapse _ = do
   ps <- io $ topTimeProcesses maxProc tref lapse
   pstr <- mapM showTimeInfo ps
   parseTemplate $ concat pstr

startTopCpu :: [String] -> Int -> (String -> IO ()) -> IO ()
startTopCpu a r cb = do
  t <- getSysVar ClockTick
  tref <- newIORef M.empty
  let lapse = (fromIntegral r * fromIntegral t) / 10
  runM a topCpuConfig (runTopCpu tref lapse) r cb
