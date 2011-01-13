module Plugins.Monitors.Volume (runVolume, volumeConfig) where

import Control.Monad ( liftM )
import Data.Maybe
import Plugins.Monitors.Common
import Sound.ALSA.Mixer

percent :: Integer -> Integer -> Integer -> Float
percent v' lo' hi' = (v - lo) / (hi - lo)
  where v = fromIntegral v'
        lo = fromIntegral lo'
        hi = fromIntegral hi'

volumeConfig :: IO MConfig
volumeConfig = mkMConfig "Vol: <volume>% <fc=green><on></fc><fc=red><off></fc>"
                         ["volume","dB","on","off"]

runVolume :: String -> String -> [String] -> Monitor String
runVolume mixerName controlName _ = do
    control <- liftM fromJust $ io $ getControlByName mixerName controlName
    let volumeControl = fromJust $ maybe (playback $ volume control) Just
                                         (common $ volume control)
        switchControl = fromJust $ maybe (playback $ switch control) Just
                                         (common $ switch control)
    (lo, hi) <- io $ getRange volumeControl
    val <- liftM fromJust $ io $ getChannel FrontLeft $ value volumeControl
    db <- liftM fromJust $ io $ getChannel FrontLeft $ dB volumeControl
    sw <- liftM fromJust $ io $ getChannel FrontLeft $ switchControl
    p <- showPercentsWithColors [ percent val lo hi ]
    let d :: Double
        d = fromIntegral db / 100.0
        dStr = showDigits 2 d
    parseTemplate $ p ++ [ dStr ]
                      ++ [ if sw then "[on] " else ""
                         , if sw then "" else "[off]"
                         ]
