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
import Data.Char (isDigit)

-- | Generate Config with a default template and options.
coreTempConfig :: IO MConfig
coreTempConfig = mkMConfig coreTempTemplate coreTempOptions
  where coreTempTemplate = "Temp: <total>°C"
        coreTempOptions = map ((++) "core" . show) [0 :: Int ..]

-- | FilePaths to read temperature from. Strings are seperated, where to
-- insert numbers.
coreTempFilePaths :: [[FilePath]]
coreTempFilePaths = [ ["/sys/bus/platform/devices/coretemp." , "/temp" , "_input"]
                    , ["/sys/bus/platform/devices/coretemp.", "/hwmon/hwmon", "/temp", "_input"]
                    ]

-- | Retrieves Monitor String holding the core temperatures.
runCoreTemp :: [String] -> Monitor String
runCoreTemp _ = do
   dn <- getConfigValue decDigits
   failureMessage <- getConfigValue naString
   let lbl  = Just ("_label", read . dropWhile (not . isDigit))
       divisor = 1e3 :: Double
       show' = showDigits (max 0 dn)
   checkedDataRetrieval failureMessage coreTempFilePaths lbl (/divisor) show'

startCoreTemp :: [String] -> Int -> (String -> IO ()) -> IO ()
startCoreTemp a r cb = runM a coreTempConfig runCoreTemp r cb
