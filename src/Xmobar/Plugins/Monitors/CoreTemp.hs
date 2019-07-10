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

-- | Generate Config with a default template and options.
coreTempConfig :: IO MConfig
coreTempConfig = mkMConfig coreTempTemplate coreTempOptions
  where coreTempTemplate = "Temp: <core0>°C"
        coreTempOptions = map (("core" ++) . show) [0 :: Int ..]

-- | FilePaths to read temperature from. Strings are seperated, where to
-- insert numbers.
coreTempFilePaths :: [[FilePath]]
coreTempFilePaths = [ ["/sys/bus/platform/devices/coretemp." , "/temp" , "_input"]
                    , ["/sys/bus/platform/devices/coretemp.", "/hwmon/hwmon", "/temp", "_input"]
                    ]

coreTempNormalize :: Double -> Double
coreTempNormalize = (/ 1000)

-- | Retrieves Monitor String holding the core temperatures.
runCoreTemp :: [String] -> Monitor String
runCoreTemp _ = do
  confDecDigits <- getConfigValue decDigits
  confNaString <- getConfigValue naString
  let coreLabel = Nothing
      coreTempShow = showDigits (max 0 confDecDigits)
  checkedDataRetrieval confNaString
                       coreTempFilePaths
                       coreLabel
                       coreTempNormalize
                       coreTempShow

startCoreTemp :: [String] -> Int -> (String -> IO ()) -> IO ()
startCoreTemp a = runM a coreTempConfig runCoreTemp
