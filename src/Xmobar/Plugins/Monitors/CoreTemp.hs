-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.CoreTemp
-- Copyright   :  (c) 2019 Felix Springer
--                (c) Juraj Hercek
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Felix Springer <felixspringer149@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A core temperature monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.CoreTemp (startCoreTemp) where

import Xmobar.Plugins.Monitors.Common
import System.Console.GetOpt

data CTOpts = CTOpts { loadIconPattern :: Maybe IconPattern
                        , mintemp :: Float
                        , maxtemp :: Float
                        }

defaultOpts :: CTOpts
defaultOpts = CTOpts { loadIconPattern = Nothing
                     , mintemp = 0
                     , maxtemp = 1
                     }

options :: [OptDescr (CTOpts -> CTOpts)]
options = [ Option [] ["load-icon-pattern"]
              (ReqArg
                (\ arg opts -> opts { loadIconPattern = Just $ parseIconPattern arg })
                "")
              ""
          , Option [] ["mintemp"]
              (ReqArg
                (\ arg opts -> opts { mintemp = read arg / 100 })
                "")
              ""
          , Option [] ["maxtemp"]
              (ReqArg
                (\ arg opts -> opts { maxtemp = read arg / 100 })
                "")
              ""
          ]

parseOpts :: [String] -> IO CTOpts
parseOpts argv = case getOpt Permute options argv of
                   (opts , _ , []  ) -> return $ foldr id defaultOpts opts
                   (_    , _ , errs) -> ioError . userError $ concat errs

-- | Generate Config with a default template and options.
cTConfig :: IO MConfig
cTConfig = mkMConfig cTTemplate cTOptions
  where cTTemplate = "Temp: <max>°C"
        cTOptions = [ "bar" , "vbar" , "ipat" , "max" , "maxpc" , "avg" , "avgpc" ] ++
                      (map (("core" ++) . show) [0 :: Int ..])

cTFilePath :: FilePath
cTFilePath = "/sys/bus/platform/devices/coretemp.0/hwmon/hwmon1/temp2_input"

cTData :: IO [Float]
cTData = do a <- readFile cTFilePath
            return $ [ parseContent a ]
  where parseContent = read . head . lines :: String -> Float

parseCT :: IO [Float]
parseCT = do rawCTs <- cTData
             let normalizedCTs = map ((/ 100000)) rawCTs :: [Float]
             return normalizedCTs

formatCT :: CTOpts -> [Float] -> Monitor [String]
formatCT opts cTs = do let CTOpts { mintemp = minT
                                  , maxtemp = maxT } = opts
                           domainT = maxT - minT
                           maxCT = maximum cTs
                           avgCT = sum cTs / (fromIntegral $ length cTs)
                           maxCTPc = (maxCT - minT) / domainT
                           avgCTPc = (avgCT - minT) / domainT

                       cTShows <- showPercentsWithColors cTs
                       cTBar <- showPercentBar (100 * maxCTPc) maxCTPc
                       cTVBar <- showVerticalBar (100 * maxCTPc) maxCTPc
                       cTIcon <- showIconPattern (loadIconPattern opts) maxCTPc
                       maxCTShow <- showPercentWithColors maxCT
                       maxCTPcShow <- showPercentWithColors maxCTPc
                       avgCTShow <- showPercentWithColors avgCT
                       avgCTPcShow <- showPercentWithColors avgCTPc

                       return (cTBar : cTVBar : cTIcon :
                         maxCTShow : maxCTPcShow :
                         avgCTShow : avgCTPcShow :
                         cTShows)

runCT :: [String] -> Monitor String
runCT argv = do cTs <- io $ parseCT
                opts <- io $ parseOpts argv
                l <- formatCT opts cTs
                parseTemplate l

startCoreTemp :: [String] -> Int -> (String -> IO ()) -> IO ()
startCoreTemp a = runM a cTConfig runCT
