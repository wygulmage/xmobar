-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Batt
-- Copyright   :  (c) Andrea Rossato, 2010 Petr Rockai, 2010 Jose A Ortega
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
import System.Console.GetOpt

data BattOpts = BattOpts
  { onString :: String
  , offString :: String
  , posColor :: Maybe String
  , lowWColor :: Maybe String
  , mediumWColor :: Maybe String
  , highWColor :: Maybe String
  , lowThreshold :: Float
  , highThreshold :: Float
  }

defaultOpts :: BattOpts
defaultOpts = BattOpts
  { onString = "On"
  , offString = "Off"
  , posColor = Nothing
  , lowWColor = Nothing
  , mediumWColor = Nothing
  , highWColor = Nothing
  , lowThreshold = -12
  , highThreshold = -10
  }

options :: [OptDescr (BattOpts -> BattOpts)]
options =
  [ Option "O" ["on"] (ReqArg (\x o -> o { onString = x }) "") ""
  , Option "o" ["off"] (ReqArg (\x o -> o { offString = x }) "") ""
  , Option "p" ["positive"] (ReqArg (\x o -> o { posColor = Just x }) "") ""
  , Option "l" ["low"] (ReqArg (\x o -> o { lowWColor = Just x }) "") ""
  , Option "m" ["medium"] (ReqArg (\x o -> o { mediumWColor = Just x }) "") ""
  , Option "h" ["high"] (ReqArg (\x o -> o { highWColor = Just x }) "") ""
  , Option "L" ["lowt"] (ReqArg (\x o -> o { lowThreshold = read x }) "") ""
  , Option "H" ["hight"] (ReqArg (\x o -> o { highThreshold = read x }) "") ""
  ]

parseOpts :: [String] -> IO BattOpts
parseOpts argv =
  case getOpt Permute options argv of
    (o, _, []) -> return $ foldr id defaultOpts o
    (_, _, errs) -> ioError . userError $ concat errs

data Result = Result Float Float Float String | NA

base :: String
base = "/sys/class/power_supply"

battConfig :: IO MConfig
battConfig = mkMConfig
       "Batt: <watts>, <left>% / <timeleft>" -- template
       ["leftbar", "left", "acstatus", "timeleft", "watts"] -- replacements

data Files = Files
  { f_full :: String
  , f_now :: String
  , f_voltage :: String
  , f_current :: String
  } | NoFiles

data Battery = Battery
  { full :: Float
  , now :: Float
  , voltage :: Float
  , current :: Float
  }

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

readBatteries :: BattOpts -> [Files] -> IO Result
readBatteries opts bfs =
    do bats <- mapM readBattery (take 3 bfs)
       ac' <- haveAc
       let ac = (ac' == Just True)
           sign = if ac then 1 else -1
           left = sum (map now bats) / sum (map full bats)
           watts = sign * sum (map voltage bats) * sum (map current bats)
           time = if watts == 0 then 0 else sum $ map time' bats -- negate sign
           time' b = (if ac then full b - now b else now b) / (sign * watts)
           acstr = case ac' of
             Nothing -> "?"
             Just True -> onString opts
             Just False -> offString opts
       return $ if isNaN left then NA else Result left watts time acstr

runBatt :: [String] -> Monitor String
runBatt = runBatt' ["BAT0","BAT1","BAT2"]

runBatt' :: [String] -> [String] -> Monitor String
runBatt' bfs args = do
  opts <- io $ parseOpts args
  c <- io $ readBatteries opts =<< mapM batteryFiles bfs
  case c of
    Result x w t s ->
      do l <- fmtPercent x
         parseTemplate (l ++ s:[fmtTime $ floor t, fmtWatts w opts])
    NA -> return "N/A"
 where fmtPercent :: Float -> Monitor [String]
       fmtPercent x = do
         p <- showPercentWithColors x
         b <- showPercentBar (100 * x) x
         return [b, p]
       fmtWatts x o = color x o $ showDigits 1 x ++ "W"
       fmtTime :: Integer -> String
       fmtTime x = hours ++ ":" ++ if length minutes == 2
                                   then minutes else '0' : minutes
         where hours = show (x `div` 3600)
               minutes = show ((x `mod` 3600) `div` 60)
       maybeColor Nothing _ = ""
       maybeColor (Just c) str = "<fc=" ++ c ++ ">" ++ str ++ "</fc>"
       color x o | x >= 0 = maybeColor (posColor o)
                 | x >= highThreshold o = maybeColor (highWColor o)
                 | x >= lowThreshold o = maybeColor (mediumWColor o)
                 | otherwise = maybeColor (lowWColor o)
