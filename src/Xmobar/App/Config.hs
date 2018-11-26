------------------------------------------------------------------------------
-- |
-- Module: Xmobar.Config.Defaults
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Nov 25, 2018 22:26
--
--
-- Default values for Xmobar configurations
--
------------------------------------------------------------------------------


module Xmobar.App.Config (defaultConfig, getXdgConfigFile) where

import System.Environment
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Xmobar.Plugins.Date
import Xmobar.Plugins.StdinReader
import Xmobar.Config.Types
import Xmobar.Run.Runnable

-- | The default configuration values
defaultConfig :: Config
defaultConfig =
    Config { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
           , additionalFonts = []
           , wmClass = "xmobar"
           , wmName = "xmobar"
           , bgColor = "#000000"
           , fgColor = "#BFBFBF"
           , alpha   = 255
           , position = Top
           , border = NoBorder
           , borderColor = "#BFBFBF"
           , borderWidth = 1
           , textOffset = -1
           , iconOffset = -1
           , textOffsets = []
           , hideOnStart = False
           , lowerOnStart = True
           , persistent = False
           , allDesktops = True
           , overrideRedirect = True
           , pickBroadest = False
           , iconRoot = "."
           , commands = [ Run $ Date "%a %b %_d %Y * %H:%M:%S" "theDate" 10
                        , Run StdinReader]
           , sepChar = "%"
           , alignSep = "}{"
           , template = "%StdinReader% }{ " ++
                        "<fc=#00FF00>%uname%</fc> * <fc=#FF0000>%theDate%</fc>"
           }

xdgConfigDir :: IO String
xdgConfigDir = do env <- getEnvironment
                  case lookup "XDG_CONFIG_HOME" env of
                       Just val -> return val
                       Nothing  -> fmap (</> ".config") getHomeDirectory

xmobarConfigDir :: IO FilePath
xmobarConfigDir = fmap (</> "xmobar") xdgConfigDir

getXdgConfigFile :: IO FilePath
getXdgConfigFile = fmap (</> "xmobarrc") xmobarConfigDir
