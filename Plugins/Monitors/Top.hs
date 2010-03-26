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

module Plugins.Monitors.Top (startTop, topMemConfig, runTopMem) where

import Plugins.Monitors.Common
import Plugins.Monitors.Mem (usedMem)

import Control.Exception (SomeException, handle, evaluate)
import System.Directory
import System.FilePath
import System.IO
import System.Posix.Unistd (getSysVar, SysVar(ClockTick))
import Foreign.C.Types
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.IORef
import Data.Time.Clock

import Data.IntMap (IntMap)
import qualified Data.IntMap as M


topMemConfig :: IO MConfig
topMemConfig = mkMConfig "<both1>" [ k ++ n | n <- map show [1..maxProc]
                                            , k <- ["name", "mem", "both"]]

topConfig :: IO MConfig
topConfig = mkMConfig "<both1>" [ k ++ n | n <- map show [1..maxProc]
                                         , k <- [ "name", "cpu", "both"
                                                , "mname", "mem", "mboth"]]



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

getProcessData :: FilePath -> IO [String]
getProcessData pidf =
  handle ((\_ -> evaluate []) :: SomeException -> IO [String])
         (withFile ("/proc" </> pidf </> "stat") ReadMode readWords)
  where readWords = fmap words . hGetLine

processes :: IO [FilePath]
processes = fmap (filter isPid) (getDirectoryContents "/proc")
  where isPid = all (`elem` ['0'..'9'])

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
        !r = pageSize * read (fs!!23)

meminfos :: IO [Meminfo]
meminfos = handleProcesses ("", 0) meminfo

topMemProcesses :: Int -> IO [Meminfo]
topMemProcesses n = fmap (take n . sbm) meminfos
  where sbm = sortBy (flip (comparing snd))

showMeminfo :: Float -> Meminfo -> Monitor [String]
showMeminfo scale (nm, rss) =
  showInfo nm sms (ms / (1024 * scale))
    where ms = fromIntegral rss
          sms = showWithUnits 2 1 ms

runTopMem :: [String] -> Monitor String
runTopMem _ = do
  ps <- io $ topMemProcesses maxProc
  pstr <- mapM (showMeminfo 1) ps
  parseTemplate $ concat pstr

type Pid = Int
type TimeInfo = (String, Float)
type TimeEntry = (Pid, TimeInfo)
type Times = IntMap TimeInfo
type TimesRef = IORef (Times, UTCTime)

timeMemEntry :: [String] -> (TimeEntry, Meminfo)
timeMemEntry fs = ((p, (n, t)), (n, r))
  where !p = read (head fs)
        !n = processName fs
        !t = read (fs!!13) + read (fs!!14)
        !r = pageSize * read (fs!!23)

timeMemEntries :: IO [(TimeEntry, Meminfo)]
timeMemEntries = handleProcesses ((0, ("", 0)), ("", 0)) timeMemEntry

timeMemInfos :: IO (Times, [Meminfo])
timeMemInfos =
  fmap (\x -> (M.fromList . map fst $ x, map snd x)) timeMemEntries

combineTimeInfos :: Times -> Times -> Times
combineTimeInfos !t0 !t1 = M.intersectionWith timeDiff t1 t0
  where timeDiff (n, x1) (_, x0) = let !d = x1 - x0 in (n, d)

topProcesses :: Int -> TimesRef -> Float -> IO ([TimeInfo], [Meminfo])
topProcesses n tref scale = do
  c1 <- getCurrentTime
  (t1, mi) <- timeMemInfos
  (t0, c0) <- readIORef tref
  writeIORef tref (t1, c1)
  let ts = M.elems $ combineTimeInfos t0 t1
      sts = take n $ sortBy (flip (comparing snd)) ts
      nts = map norm sts
      scx = (fromRational . toRational $! diffUTCTime c1 c0) * scale / 100
      norm (nm, t) = (nm, t / scx)
      mis = take n (sbm mi)
      sbm = sortBy (flip (comparing snd))
  return (nts, mis)

showTimeInfo :: TimeInfo -> Monitor [String]
showTimeInfo (n, t) = showInfo n (showDigits 1 t) t

runTop :: TimesRef -> Float -> Float -> [String] -> Monitor String
runTop tref scale mscale _ = do
  (ps, ms) <- io $ topProcesses maxProc tref scale
  pstr <- mapM showTimeInfo ps
  mstr <- mapM (showMeminfo mscale) ms
  parseTemplate $ concat $ zipWith (++) pstr mstr

startTop :: [String] -> Int -> (String -> IO ()) -> IO ()
startTop a r cb = do
  cr <- getSysVar ClockTick
  m <- usedMem
  c <- getCurrentTime
  tref <- newIORef (M.empty, c)
  runM a topConfig (runTop tref (fromIntegral cr) m) r cb
