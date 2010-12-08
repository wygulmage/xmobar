-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Batt
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A battery monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Batt ( battConfig, runBatt, runBatt' ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Plugins.Monitors.Common
import System.Posix.Files (fileExist)

data Result = Result { percent :: Float, watts :: Float, time :: Float, ac :: String }
            | NA

base = "/sys/class/power_supply"

battConfig :: IO MConfig
battConfig = mkMConfig
       "Batt: <watts>, <left> / <timeleft>" -- template
       ["leftbar", "left", "status", "timeleft", "watts"] -- available replacements

data Files = Files { f_full :: String, f_now :: String
                   , f_voltage :: String, f_current :: String } | NoFiles
data Battery = Battery { full :: Float, now :: Float, voltage :: Float, current :: Float }

batteryFiles :: String -> IO Files
batteryFiles bat =
  do is_charge <- fileExist $ prefix ++ "/charge_now"
     is_energy <- fileExist $ prefix ++ "/energy_now"
     return $ case (is_charge, is_energy) of
       (True, _) -> files "/charge"
       (_, True) -> files "/energy"
       _ -> NoFiles
  where prefix = base ++ "/" ++ bat
        files ch = Files { f_full = prefix ++ ch ++ "_full"
                         , f_now = prefix ++ ch ++ "_now"
                         , f_current = prefix ++ "/current_now"
                         , f_voltage = prefix ++ "/voltage_now" }

haveAc :: IO (Maybe Bool)
haveAc = do know <- fileExist $ base ++ "/AC/online"
            if know
               then do s <- B.unpack `fmap` catRead (base ++ "/AC/online")
                       return $ Just $ s == "1\n"
               else return Nothing

readBattery :: Files -> IO Battery
readBattery NoFiles = return $ Battery 0 0 0 0
readBattery files =
    do a <- grab $ f_full files -- microwatthours
       b <- grab $ f_now files
       c <- grab $ f_voltage files -- microvolts
       d <- grab $ f_current files -- microwatts (huh!)
       return $ Battery (3600 * a / 1000000) -- wattseconds
                        (3600 * b / 1000000) -- wattseconds
                        (c / 1000000) -- volts
                        (d / c) -- amperes
    where grab = fmap (read . B.unpack) . catRead

readBatteries :: [Files] -> IO Result
readBatteries bfs =
    do bats <- mapM readBattery (take 3 bfs)
       ac' <- haveAc
       let ac = if ac' == Just True then True else False
           sign = if ac then 1 else -1
           left = (sum $ map now bats) / (sum $ map full bats)
           watts = sign * (sum $ map voltage bats) * (sum $ map current bats)
           time = if watts == 0 then 0 else sum $ map time' bats -- negate sign
           time' b = (if ac then full b - now b else now b) / (sign * watts)
           acstr = case ac' of
             Nothing -> "?"
             Just True -> "<fc=green>On</fc>"
             Just False -> "<fc=red>Off</fc>"
       return $ if isNaN left then NA else Result left watts time acstr

runBatt :: [String] -> Monitor String
runBatt = runBatt' ["BAT0","BAT1","BAT2"]

runBatt' :: [String] -> [String] -> Monitor String
runBatt' bfs _ = do
  c <- io $ readBatteries =<< mapM batteryFiles bfs
  case c of
    Result x w t s -> do l <- fmtPercent x
                         parseTemplate (l ++ [s] ++ [fmtTime $ floor t, fmtWatts w])
    NA -> return "N/A"
 where fmtPercent :: Float -> Monitor [String]
       fmtPercent x = do
         p <- showPercentsWithColors [x]
         b <- showPercentBar (100 * x) x
         return (b:p)
       fmtWatts x = color x $ showDigits 1 x ++ "W"
       fmtTime x = hours ++ ":" ++ if length minutes == 2 then minutes else "0" ++ minutes
         where hours = show (x `div` 3600)
               minutes = show ((x `mod` 3600) `div` 60)
       color x str | x >= 0 = "<fc=orange>" ++ str ++ "</fc>"
                   | x >= -10 = "<fc=green>" ++ str ++ "</fc>"
                   | x >= -12 = str
                   | otherwise = "<fc=red>" ++ str ++ "</fc>"
