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
              , "lapsed", "remaining", "plength", "ppos"
              , "name", "artist", "composer", "performer"
              , "album", "title", "track", "trackno", "file", "genre"
              ]

data MOpts = MOpts {mPlaying :: String, mStopped :: String, mPaused :: String}

defaultOpts :: MOpts
defaultOpts = MOpts { mPlaying = ">>", mStopped = "><", mPaused = "||" }

options :: [OptDescr (MOpts -> MOpts)]
options =
  [ Option "P" ["playing"] (ReqArg (\x o -> o { mPlaying = x }) "") ""
  , Option "S" ["stopped"] (ReqArg (\x o -> o { mStopped = x }) "") ""
  , Option "Z" ["paused"] (ReqArg (\x o -> o { mPaused = x }) "") ""
  ]

runMPD :: [String] -> Monitor String
runMPD args = do
  status <- io $ M.withMPD M.status
  song <- io $ M.withMPD M.currentSong
  opts <- io $ mopts args
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
  (b, [ss, si, vol, len, lap, remain, plen, pp] ++ sf)
  where s = M.stState st
        ss = show s
        si = stateGlyph s opts
        vol = int2str $ M.stVolume st
        (p, t) = M.stTime st
        (lap, len) = (showTime p, showTime t)
        remain = showTime $ max 0 (t - p)
        b = if t > 0 then fromIntegral p / fromIntegral t else 0
        plen = int2str $ M.stPlaylistLength st
        sf = parseSong song
        pp = case M.stSongPos st of
               Nothing -> ""
               Just (M.Pos n) -> int2str $ n + 1
               Just (M.ID n) -> int2str n

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
  [ M.sgName s, M.sgArtist s, M.sgComposer s, M.sgPerformer s
  , M.sgAlbum s, M.sgTitle s, track, trackno, M.sgFilePath s, M.sgGenre s]
  where (track, trackno) = (show t, show tn)
        (t, tn) = M.sgTrack s

showTime :: Integer -> String
showTime t = int2str minutes ++ ":" ++ int2str seconds
  where minutes = t `div` 60
        seconds = t `mod` 60

int2str :: (Num a, Ord a) => a -> String
int2str x = if x < 10 then '0':sx else sx where sx = show x
