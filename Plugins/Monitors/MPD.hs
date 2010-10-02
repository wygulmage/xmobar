-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.MPD
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
--  MPD status and song
--
-----------------------------------------------------------------------------

module Plugins.Monitors.MPD ( mpdConfig, runMPD ) where

import Plugins.Monitors.Common
import System.Console.GetOpt
import qualified Network.MPD as M

mpdConfig :: IO MConfig
mpdConfig = mkMConfig "MPD: <state>"
              [ "bar", "state", "statei", "volume", "length"
              , "lapsed", "remaining", "plength", "ppos", "file"
              , "name", "artist", "composer", "performer"
              , "album", "title", "track", "genre"
              ]

data MOpts = MOpts
  { mPlaying :: String
  , mStopped :: String
  , mPaused :: String
  , mHost :: String
  , mPort :: Integer
  , mPassword :: String
  }

defaultOpts :: MOpts
defaultOpts = MOpts
  { mPlaying = ">>"
  , mStopped = "><"
  , mPaused = "||"
  , mHost = "127.0.0.1"
  , mPort = 6600
  , mPassword = ""
  }

options :: [OptDescr (MOpts -> MOpts)]
options =
  [ Option "P" ["playing"] (ReqArg (\x o -> o { mPlaying = x }) "") ""
  , Option "S" ["stopped"] (ReqArg (\x o -> o { mStopped = x }) "") ""
  , Option "Z" ["paused"] (ReqArg (\x o -> o { mPaused = x }) "") ""
  , Option "h" ["host"] (ReqArg (\x o -> o { mHost = x }) "") ""
  , Option "p" ["port"] (ReqArg (\x o -> o { mPort = read x }) "") ""
  , Option "x" ["password"] (ReqArg (\x o -> o { mPassword = x }) "") ""
  ]

runMPD :: [String] -> Monitor String
runMPD args = do
  opts <- io $ mopts args
  let mpd = M.withMPDEx (mHost opts) (mPort opts) (mPassword opts)
  status <- io $ mpd M.status
  song <- io $ mpd M.currentSong
  let (b, s) = parseMPD status song opts
  bs <- showPercentBar (100 * b) b
  parseTemplate (bs:s)

mopts :: [String] -> IO MOpts
mopts argv =
  case getOpt Permute options argv of
    (o, _, []) -> return $ foldr id defaultOpts o
    (_, _, errs) -> ioError . userError $ concat errs

parseMPD :: M.Response M.Status -> M.Response (Maybe M.Song) -> MOpts
            -> (Float, [String])
parseMPD (Left e) _ _ = (0, show e:repeat "")
parseMPD (Right st) song opts =
  (b, [ss, si, vol, len, lap, remain, plen, pp] ++ parseSong song)
  where s = M.stState st
        ss = show s
        si = stateGlyph s opts
        vol = int2str $ M.stVolume st
        (p, t) = M.stTime st
        ps = floor p
        [lap, len, remain] = map showTime [ps, t, max 0 (t - ps)]
        b = if t > 0 then realToFrac $ p / fromIntegral t else 0
        plen = int2str $ M.stPlaylistLength st
        pp = case M.stSongPos st of
               Nothing -> ""
               Just n -> int2str $ n + 1

stateGlyph :: M.State -> MOpts -> String
stateGlyph s o =
  case s of
    M.Playing -> mPlaying o
    M.Paused -> mPaused o
    M.Stopped -> mStopped o

parseSong :: M.Response (Maybe M.Song) -> [String]
parseSong (Left _) = repeat ""
parseSong (Right Nothing) = repeat ""
parseSong (Right (Just s)) =
  M.sgFilePath s : map str [ M.Name, M.Artist, M.Composer, M.Performer
                           , M.Album, M.Title, M.Track, M.Genre ]
  where join [] = ""
        join (x:xs) = foldl (\a o -> a ++ ", " ++ o) x xs
        str sel = maybe "" join (M.sgGet sel s)

showTime :: Integer -> String
showTime t = int2str minutes ++ ":" ++ int2str seconds
  where minutes = t `div` 60
        seconds = t `mod` 60

int2str :: (Num a, Ord a) => a -> String
int2str x = if x < 10 then '0':sx else sx where sx = show x
