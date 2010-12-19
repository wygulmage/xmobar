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

module Plugins.Monitors.Top (startTop, topMemConfig, runTopMem) where

import Plugins.Monitors.Common

import Control.Exception (SomeException, handle)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import System.IO (IOMode(ReadMode), hGetLine, withFile)
import System.Posix.Unistd (SysVar(ClockTick), getSysVar)

import Foreign.C.Types

maxEntries :: Int
maxEntries = 10

intStrs :: [String]
intStrs = map show [1..maxEntries]

topMemConfig :: IO MConfig
topMemConfig = mkMConfig "<both1>"
                 [ k ++ n | n <- intStrs , k <- ["name", "mem", "both"]]

topConfig :: IO MConfig
topConfig = mkMConfig "<both1>"
              ("no" : [ k ++ n | n <- intStrs
                               , k <- [ "name", "cpu", "both"
                                      , "mname", "mem", "mboth"]])

foreign import ccall "unistd.h getpagesize"
  c_getpagesize :: CInt

pageSize :: Float
pageSize = fromIntegral c_getpagesize / 1024

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

ignoreErrors :: IO [String] -> IO [String]
ignoreErrors = handle returnEmpty
  where returnEmpty = const (return []) :: SomeException -> IO [String]

processes :: IO [FilePath]
processes = ignoreErrors $ fmap (filter isPid) (getDirectoryContents "/proc")
  where isPid = (`elem` ['0'..'9']) . head

getProcessData :: FilePath -> IO [String]
getProcessData pidf =
  ignoreErrors $ withFile ("/proc" </> pidf </> "stat") ReadMode readWords
  where readWords = fmap words . hGetLine

handleProcesses :: ([String] -> a) -> IO [a]
handleProcesses f =
  fmap (foldl' (\a p -> if null p then a else f p : a) [])
       (processes >>= mapM getProcessData)

processName :: [String] -> String
processName = drop 1 . init . (!!1)

sortTop :: [(a, Float)] -> [(a, Float)]
sortTop =  sortBy (flip (comparing snd))

type Meminfo = (String, Float)

meminfo :: [String] -> Meminfo
meminfo fs = (processName fs, pageSize * read (fs!!23))

meminfos :: IO [Meminfo]
meminfos = handleProcesses meminfo

showMeminfo :: Float -> Meminfo -> Monitor [String]
showMeminfo scale (nm, rss) =
  showInfo nm (showWithUnits 2 1 rss) (100 * rss / sc)
  where sc = if scale > 0 then scale else 100

showMeminfos :: [Meminfo] -> Monitor [[String]]
showMeminfos ms = mapM (showMeminfo tm) $ sortTop ms
  where tm = sum (map snd ms)

runTopMem :: [String] -> Monitor String
runTopMem _ = do
  pstr <- io meminfos >>= showMeminfos
  parseTemplate $ concat pstr

type Pid = Int
type TimeInfo = (String, Float)
type TimeEntry = (Pid, TimeInfo)
type Times = [TimeEntry]
type TimesRef = IORef (Times, UTCTime)

timeMemEntry :: [String] -> (TimeEntry, Meminfo)
timeMemEntry fs = ((p, (n, t)), (n, r))
  where p = read (head fs)
        n = processName fs
        t = read (fs!!13) + read (fs!!14)
        (_, r) = meminfo fs

timeMemEntries :: IO [(TimeEntry, Meminfo)]
timeMemEntries = handleProcesses timeMemEntry

timeMemInfos :: IO (Times, [Meminfo], Int)
timeMemInfos =
  fmap (\x -> (sortPids $ map fst x, map snd x, length x)) timeMemEntries
  where sortPids =  sortBy (comparing fst)

combine :: Times -> Times -> Times
combine _ [] = []
combine [] t = t
combine l@((p0, (n0, t0)):xs) r@((p1, (n1, t1)):ys)
  | p0 == p1 = (p0, (n0, t1 - t0)) : combine xs ys
  | p0 < p1 = combine xs r
  | otherwise = (p1, (n1, t1)) : combine l ys

topProcesses :: TimesRef -> Float -> IO (Int, [TimeInfo], [Meminfo])
topProcesses tref scale = do
  (t0, c0) <- readIORef tref
  (t1', mis, len) <- timeMemInfos
  c1 <- getCurrentTime
  let !t1 = t1'
  writeIORef tref (t1, c1)
  let scx = realToFrac (diffUTCTime c1 c0) * scale
      -- c0 and c1 can be equal, for instance, if we tweak the clock
      !scx' = if scx > 0 then scx else scale
      ts = combine t0 t1
      nts = map (\(_, (nm, t)) -> (nm, min 100 (t / scx'))) ts
  return (len, sortTop nts, mis)

showTimeInfo :: TimeInfo -> Monitor [String]
showTimeInfo (n, t) = showInfo n (showDigits 0 t) t

runTop :: TimesRef -> Float -> [String] -> Monitor String
runTop tref scale _ = do
  (no, ps, ms) <- io $ topProcesses tref scale
  pstr <- mapM showTimeInfo ps
  mstr <- showMeminfos ms
  let !pstr' = take maxEntries pstr
      !mstr' = take maxEntries mstr
  parseTemplate $! show no : concat (zipWith (++) pstr' mstr')

startTop :: [String] -> Int -> (String -> IO ()) -> IO ()
startTop a r cb = do
  cr <- getSysVar ClockTick
  c <- getCurrentTime
  tref <- newIORef ([], c)
  runM a topConfig (runTop tref (fromIntegral cr / 100)) r cb
