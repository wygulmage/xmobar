module Plugins.Monitors.Volume (runVolume, volumeConfig) where

import Control.Monad ( liftM )
import Data.Maybe
import Plugins.Monitors.Common
import Sound.ALSA.Mixer
import System.Console.GetOpt

data VolumeOpts = VolumeOpts
    { onString :: String
    , offString :: String
    , onColor :: Maybe String
    , offColor :: Maybe String
    , highDbThresh :: Float
    , lowDbThresh :: Float
    }

defaultOpts :: VolumeOpts
defaultOpts = VolumeOpts
    { onString = "[on] "
    , offString = "[off]"
    , onColor = Just "green"
    , offColor = Just "red"
    , highDbThresh = -5.0
    , lowDbThresh = -30.0
    }

options :: [OptDescr (VolumeOpts -> VolumeOpts)]
options =
    [ Option "O" ["on"] (ReqArg (\x o -> o { onString = x }) "") ""
    , Option "o" ["off"] (ReqArg (\x o -> o { offString = x }) "") ""
    , Option "" ["lowd"] (ReqArg (\x o -> o { lowDbThresh = read x }) "") ""
    , Option "" ["highd"] (ReqArg (\x o -> o { highDbThresh = read x }) "") ""
    , Option "C" ["onc"] (ReqArg (\x o -> o { onColor = Just x }) "") ""
    , Option "c" ["offc"] (ReqArg (\x o -> o { offColor = Just x }) "") ""
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

volumeConfig :: IO MConfig
volumeConfig = mkMConfig "Vol: <volume>% <status>"
                         ["volume","dB","status"]

formatVol :: Integer -> Integer -> Integer -> Monitor String
formatVol v lo hi =
    showPercentWithColors $ percent v lo hi

switchHelper :: VolumeOpts
             -> (VolumeOpts -> Maybe String)
             -> (VolumeOpts -> String)
             -> Monitor String
switchHelper opts cHelp strHelp = return $
    (colorHelper $ cHelp opts)
    ++ strHelp opts
    ++ (maybe "" (const "</fc>") $ cHelp opts)

formatSwitch :: VolumeOpts -> Bool -> Monitor String
formatSwitch opts True = switchHelper opts onColor onString
formatSwitch opts False = switchHelper opts offColor offString

colorHelper :: Maybe String -> String
colorHelper = maybe "" (\c -> "<fc=" ++ c ++ ">")

formatDb :: VolumeOpts -> Float -> Monitor String
formatDb opts db = do
    h <- getConfigValue highColor
    m <- getConfigValue normalColor
    l <- getConfigValue lowColor
    let digits = showDigits 0 db
        startColor | db >= highDbThresh opts = colorHelper h
                   | db < lowDbThresh opts = colorHelper l
                   | otherwise = colorHelper m
        stopColor | null startColor = ""
                  | otherwise = "</fc>"
    return $ startColor ++ digits ++ stopColor

runVolume :: String -> String -> [String] -> Monitor String
runVolume mixerName controlName argv = do
    opts <- io $ parseOpts argv
    control <- liftM fromJust $ io $ getControlByName mixerName controlName
    let volumeControl = fromJust $ maybe (playback $ volume control) Just
                                         (common $ volume control)
        switchControl = fromJust $ maybe (playback $ switch control) Just
                                         (common $ switch control)
    (lo, hi) <- io $ getRange volumeControl
    val <- io $ getChannel FrontLeft $ value volumeControl
    db <- io $ getChannel FrontLeft $ dB volumeControl
    sw <- io $ getChannel FrontLeft $ switchControl
    p <- case val of
             Just x -> formatVol x lo hi
             Nothing -> formatVol hi lo hi
    d <- case db of
             Just x -> formatDb opts $ fromIntegral x / 100.0
             Nothing -> formatDb opts 0.0
    s <- case sw of
             Just x -> formatSwitch opts x
             Nothing -> formatSwitch opts True
    parseTemplate $ [ p, d, s ] 
