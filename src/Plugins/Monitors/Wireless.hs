-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Wireless
-- Copyright   :  (c) Jose Antonio Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose Antonio Ortega Ruiz
-- Stability   :  unstable
-- Portability :  unportable
--
-- A monitor reporting ESSID and link quality for wireless interfaces
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Wireless (wirelessConfig, runWireless)  where

import System.Console.GetOpt

import Plugins.Monitors.Common
import IWlib

data WirelessOpts = WirelessOpts
  { qualityDynamicString :: Maybe DynamicString
  }

defaultOpts :: WirelessOpts
defaultOpts = WirelessOpts
  { qualityDynamicString = Nothing
  }

options :: [OptDescr (WirelessOpts -> WirelessOpts)]
options =
  [ Option "" ["quality-dynamic-string"] (ReqArg (\d opts ->
     opts { qualityDynamicString = Just $ parseDynamicString d }) "") ""
  ]

parseOpts :: [String] -> IO WirelessOpts
parseOpts argv =
  case getOpt Permute options argv of
       (o, _, []) -> return $ foldr id defaultOpts o
       (_, _, errs) -> ioError . userError $ concat errs

wirelessConfig :: IO MConfig
wirelessConfig =
  mkMConfig "<essid> <quality>" ["essid", "quality", "qualitybar", "qualityvbar", "qualitydstr"]

runWireless :: String -> [String] -> Monitor String
runWireless iface args = do
  opts <- io $ parseOpts args
  wi <- io $ getWirelessInfo iface
  na <- getConfigValue naString
  let essid = wiEssid wi
      qlty = fromIntegral $ wiQuality wi
      e = if essid == "" then na else essid
  ep <- showWithPadding e
  q <- if qlty >= 0
       then showPercentWithColors (qlty / 100)
       else showWithPadding ""
  qb <- showPercentBar qlty (qlty / 100)
  qvb <- showVerticalBar qlty (qlty / 100)
  qdstr <- showDynamicString (qualityDynamicString opts) (qlty / 100)
  parseTemplate [ep, q, qb, qvb, qdstr]
