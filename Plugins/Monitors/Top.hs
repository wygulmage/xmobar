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
import System.IO
import System.Posix.Unistd (getSysVar, SysVar(ClockTick))
import Foreign.C.Types
import Data.List (sortBy)
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

strictReadFile :: FilePath -> IO String
strictReadFile f =
  do hdl <- openFile f ReadMode
     xs <- getc hdl
     hClose hdl
     return xs
    where getc hdl = do e <- hIsEOF hdl
                        if e then return []
                          else do c <- hGetChar hdl
                                  cs <- getc hdl
                                  return (c:cs)

getProcessData :: FilePath -> IO [String]
getProcessData pidf =
  handle ((\_ -> evaluate []) :: SomeException -> IO [String])
         (do s <- strictReadFile $ "/proc" </> pidf </> "stat"
             evaluate $! words s)

processes :: IO [FilePath]
processes = do
  fs <- getDirectoryContents "/proc"
  return $! filter isPid $! fs
  where
    isPid (x:_) = x `elem` ['0'..'9']
    isPid _ = False

handleProcesses :: a -> ([String] -> a) -> IO [a]
handleProcesses def f = do
  ps <- processes
  pd <- mapM getProcessData $! ps
  return $! map (\x -> if x == [] then def else f x) pd

processName :: [String] -> String
processName = drop 1 . init . (!!1)

maxProc :: Int
maxProc = 5

type Meminfo = (String, Int)

meminfo :: [String] -> Meminfo
meminfo fs = (n, r)
  where !n = processName fs
        !r = pageSize * (read (fs!!23))

meminfos :: IO [Meminfo]
meminfos = handleProcesses ("", 0) meminfo

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

timeEntry :: [String] -> TimeEntry
timeEntry fs = (p, (n, t))
  where !p = read (head fs)
        !n = processName fs
        !t = read (fs!!13) + read (fs!!14)

timeEntries :: IO [TimeEntry]
timeEntries = handleProcesses (0, ("", 0)) timeEntry

timeinfos :: IO Times
timeinfos = fmap M.fromList timeEntries

combineTimeInfos :: Times -> Times -> Times
combineTimeInfos !t0 !t1 = M.intersectionWith timeDiff t1 t0
  where timeDiff (n, x1) (_, x0) = (n, x1 - x0)

topTimeProcesses :: Int -> TimesRef -> Float -> IO [TimeInfo]
topTimeProcesses n tref lapse = do
  t1 <- timeinfos
  t0 <- readIORef tref
  modifyIORef tref (const $! t1)
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
  cr <- getSysVar ClockTick
  tref <- newIORef M.empty
  let lapse = (fromIntegral r * fromIntegral cr) / 10
  runM a topCpuConfig (runTopCpu tref lapse) r cb
