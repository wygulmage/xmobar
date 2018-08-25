-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Volume
-- Copyright   :  (c) 2011, 2013, 2015 Thomas Tuegel
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A monitor for ALSA soundcards
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Volume
  ( startVolume
  , runVolume
  , volumeConfig
  , getMonitorWaiter
  , parseOptsIncludingMonitorArgs
  , VolumeOpts(refreshMode)
  , RefreshMode(..)
  ) where

import Commands (tenthSeconds)
import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Exception
import Control.Monad ( forever, liftM2, liftM3, mplus, void, when, unless )
import Data.Traversable (sequenceA)
import Plugins.Monitors.Common
import Sound.ALSA.Mixer
import qualified Sound.ALSA.Exception as AE
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.IO
import System.Process

volumeConfig :: IO MConfig
volumeConfig = mkMConfig "Vol: <volume>% <status>"
                         ["volume", "volumebar", "volumevbar", "dB","status", "volumeipat"]


data VolumeOpts = VolumeOpts
    { onString :: String
    , offString :: String
    , onColor :: Maybe String
    , offColor :: Maybe String
    , highDbThresh :: Float
    , lowDbThresh :: Float
    , volumeIconPattern :: Maybe IconPattern
    , refreshMode :: RefreshMode
    }

data RefreshMode = RefreshModePoll
                 | RefreshModeMonitor (Maybe FilePath) -- alsactl path
                 deriving(Eq,Show)

defaultOpts :: VolumeOpts
defaultOpts = VolumeOpts
    { onString = "[on] "
    , offString = "[off]"
    , onColor = Just "green"
    , offColor = Just "red"
    , highDbThresh = -5.0
    , lowDbThresh = -30.0
    , volumeIconPattern = Nothing
    , refreshMode = RefreshModePoll
    }

monitorOptionName :: String
monitorOptionName = "monitor"

options :: [OptDescr (VolumeOpts -> VolumeOpts)]
options =
    [ Option "O" ["on"] (ReqArg (\x o -> o { onString = x }) "") ""
    , Option "o" ["off"] (ReqArg (\x o -> o { offString = x }) "") ""
    , Option "" ["lowd"] (ReqArg (\x o -> o { lowDbThresh = read x }) "") ""
    , Option "" ["highd"] (ReqArg (\x o -> o { highDbThresh = read x }) "") ""
    , Option "C" ["onc"] (ReqArg (\x o -> o { onColor = Just x }) "") ""
    , Option "c" ["offc"] (ReqArg (\x o -> o { offColor = Just x }) "") ""
    , Option "" ["volume-icon-pattern"] (ReqArg (\x o ->
       o { volumeIconPattern = Just $ parseIconPattern x }) "") ""
    , Option "" [monitorOptionName] (OptArg (\x o ->
       o { refreshMode = RefreshModeMonitor x }) "") ""
    ]

parseOpts :: [String] -> IO VolumeOpts
parseOpts argv =
    case getOpt Permute options argv of
        (o, _, []) -> return $ foldr id defaultOpts o
        (_, _, errs) -> ioError . userError $ concat errs

percent :: Integer -> Integer -> Integer -> Float
percent v' lo' hi' = (v - lo) / (hi - lo)
  where v = fromIntegral v'
        lo = fromIntegral lo'
        hi = fromIntegral hi'

formatVol :: Integer -> Integer -> Integer -> Monitor String
formatVol lo hi v =
    showPercentWithColors $ percent v lo hi

formatVolBar :: Integer -> Integer -> Integer -> Monitor String
formatVolBar lo hi v =
    showPercentBar (100 * x) x where x = percent v lo hi

formatVolVBar :: Integer -> Integer -> Integer -> Monitor String
formatVolVBar lo hi v =
    showVerticalBar (100 * x) x where x = percent v lo hi

formatVolDStr :: Maybe IconPattern -> Integer -> Integer -> Integer -> Monitor String
formatVolDStr ipat lo hi v =
    showIconPattern ipat $ percent v lo hi

switchHelper :: VolumeOpts
             -> (VolumeOpts -> Maybe String)
             -> (VolumeOpts -> String)
             -> Monitor String
switchHelper opts cHelp strHelp = return $
    colorHelper (cHelp opts)
    ++ strHelp opts
    ++ maybe "" (const "</fc>") (cHelp opts)

formatSwitch :: VolumeOpts -> Bool -> Monitor String
formatSwitch opts True = switchHelper opts onColor onString
formatSwitch opts False = switchHelper opts offColor offString

colorHelper :: Maybe String -> String
colorHelper = maybe "" (\c -> "<fc=" ++ c ++ ">")

formatDb :: VolumeOpts -> Integer -> Monitor String
formatDb opts dbi = do
    h <- getConfigValue highColor
    m <- getConfigValue normalColor
    l <- getConfigValue lowColor
    d <- getConfigValue decDigits
    let db = fromIntegral dbi / 100.0
        digits = showDigits d db
        startColor | db >= highDbThresh opts = colorHelper h
                   | db < lowDbThresh opts = colorHelper l
                   | otherwise = colorHelper m
        stopColor | null startColor = ""
                  | otherwise = "</fc>"
    return $ startColor ++ digits ++ stopColor

runVolume :: String -> String -> [String] -> Monitor String
runVolume mixerName controlName argv = do
    opts <- io $ parseOpts argv
    (lo, hi, val, db, sw) <- io readMixer
    p <- liftMonitor $ liftM3 formatVol lo hi val
    b <- liftMonitor $ liftM3 formatVolBar lo hi val
    v <- liftMonitor $ liftM3 formatVolVBar lo hi val
    d <- getFormatDB opts db
    s <- getFormatSwitch opts sw
    ipat <- liftMonitor $ liftM3 (formatVolDStr $ volumeIconPattern opts) lo hi val
    parseTemplate [p, b, v, d, s, ipat]

  where

    readMixer =
      AE.catch (withMixer mixerName $ \mixer -> do
                   control <- getControlByName mixer controlName
                   (lo, hi) <- liftMaybe $ getRange <$> volumeControl control
                   val <- getVal $ volumeControl control
                   db <- getDB $ volumeControl control
                   sw <- getSw $ switchControl control
                   return (lo, hi, val, db, sw))
                (const $ return (Nothing, Nothing, Nothing, Nothing, Nothing))

    volumeControl :: Maybe Control -> Maybe Volume
    volumeControl c = (playback . volume =<< c)
              `mplus` (capture . volume =<< c)
              `mplus` (common . volume =<< c)

    switchControl :: Maybe Control -> Maybe Switch
    switchControl c = (playback . switch =<< c)
              `mplus` (capture . switch =<< c)
              `mplus` (common . switch =<< c)

    liftMaybe :: Maybe (IO (a,b)) -> IO (Maybe a, Maybe b)
    liftMaybe = fmap (liftM2 (,) (fmap fst) (fmap snd)) . sequenceA

    liftMonitor :: Maybe (Monitor String) -> Monitor String
    liftMonitor Nothing = unavailable
    liftMonitor (Just m) = m

    channel v r = AE.catch (getChannel FrontLeft v) (const $ return $ Just r)

    getDB :: Maybe Volume -> IO (Maybe Integer)
    getDB Nothing = return Nothing
    getDB (Just v) = channel (dB v) 0

    getVal :: Maybe Volume -> IO (Maybe Integer)
    getVal Nothing = return Nothing
    getVal (Just v) = channel (value v) 0

    getSw :: Maybe Switch -> IO (Maybe Bool)
    getSw Nothing = return Nothing
    getSw (Just s) = channel s False

    getFormatDB :: VolumeOpts -> Maybe Integer -> Monitor String
    getFormatDB _ Nothing = unavailable
    getFormatDB opts (Just d) = formatDb opts d

    getFormatSwitch :: VolumeOpts -> Maybe Bool -> Monitor String
    getFormatSwitch _ Nothing = unavailable
    getFormatSwitch opts (Just sw) = formatSwitch opts sw

    unavailable = getConfigValue naString

parseOptsIncludingMonitorArgs :: [String] -> IO VolumeOpts
parseOptsIncludingMonitorArgs args =
    -- Drop generic Monitor args first
    case getOpt Permute [] args of
      (_, args', _) -> parseOpts args'

startVolume :: String -> String -> [String] -> Int -> (String -> IO ()) -> IO ()
startVolume mixerName controlName args rate cb = do
  opts <- parseOptsIncludingMonitorArgs args

  waitFunction <-
    case refreshMode opts of
      RefreshModePoll -> pure $ tenthSeconds rate
      RefreshModeMonitor alsaCtlPath -> getMonitorWaiter mixerName alsaCtlPath

  runMB args volumeConfig (runVolume mixerName controlName) waitFunction cb


getMonitorWaiter :: String -> Maybe FilePath -> IO (IO ())
getMonitorWaiter mixerName alsaCtlPath = do
  mvar <- newMVar Nothing :: IO (MVar (Maybe SomeException))

  forkFinally (readerThread mvar) (putMVar mvar . either Just (const Nothing))

  pure $ do
    ei <- takeMVar mvar
    case ei of
      -- Propagate exceptions from reader thread
      Just (SomeException ex) -> throwIO ex
      Nothing -> pure ()

  where

    readerThread mvar = do
          path <- determineAlsaCtlPath
          withCreateProcess
            (proc "stdbuf" ["-oL", path, "monitor", mixerName]) {std_out = CreatePipe}
            run

      where

        defaultPath = "/usr/sbin/alsactl"

        determineAlsaCtlPath =
          case alsaCtlPath of
            Just path -> do
              found <- doesFileExist path
              if found
                then pure path
                else throwIO . ErrorCall $
                     "Specified alsactl file " ++ path ++ " does not exist"

            Nothing -> do
              (ec, path, err) <- readProcessWithExitCode "which" ["alsactl"] ""
              unless (null err) $ hPutStrLn stderr err
              case ec of
                ExitSuccess -> pure $ trimTrailingNewline path
                ExitFailure _ -> do
                  found <- doesFileExist defaultPath
                  if found
                    then pure defaultPath
                    else throwIO . ErrorCall $
                         "alsactl not found in PATH or at " ++
                         show defaultPath ++
                         "; please specify with --" ++
                         monitorOptionName ++ "=/path/to/alsactl"


        run _ ~(Just out) _ _ = do
          hSetBuffering out LineBuffering
          forever $ do
            c <- hGetChar out
            when (c == '\n') $
              -- This uses 'tryPutMVar' because 'putMVar' would make 'runVolume' run
              -- once for each event. But we want it to run only once after a burst
              -- of events.
              void $ tryPutMVar mvar Nothing

-- This is necessarily very inefficient on 'String's
trimTrailingNewline :: String -> String
trimTrailingNewline x =
  case reverse x of
    '\n' : '\r' : y -> reverse y
    '\n' : y -> reverse y
    _ -> x
