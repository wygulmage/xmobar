------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.Main
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Nov 25, 2018 21:53
--
--
-- Support for creating executable main functions
--
------------------------------------------------------------------------------


module Xmobar.App.Main (xmobar) where

import Data.Foldable (for_)
import qualified Data.Map as Map

import Graphics.X11.Xlib
import Control.Concurrent.Async (Async, cancel)
import Control.Exception (bracket)

import Xmobar.Config.Types
import Xmobar.System.Signal (setupSignalHandler, withDeferSignals)
import Xmobar.Run.Template
import Xmobar.X11.Types
import Xmobar.X11.Text
import Xmobar.X11.Window
import Xmobar.App.EventLoop (startLoop, startCommand)

xmobar :: Config -> IO ()
xmobar conf = withDeferSignals $ do
  initThreads
  d <- openDisplay ""
  fs    <- initFont d (font conf)
  fl    <- mapM (initFont d) (additionalFonts conf)
  cls   <- mapM (parseTemplate (commands conf) (sepChar conf))
                (splitTemplate (alignSep conf) (template conf))
  sig   <- setupSignalHandler
  bracket (mapM (mapM $ startCommand sig) cls)
          cleanupThreads
          $ \vars -> do
    (r,w) <- createWin d fs conf
    let ic = Map.empty
        to = textOffset conf
        ts = textOffsets conf ++ replicate (length fl) (-1)
    startLoop (XConf d r w (fs:fl) (to:ts) ic conf) sig vars

cleanupThreads :: [[([Async ()], a)]] -> IO ()
cleanupThreads vars =
  for_ (concat vars) $ \(asyncs, _) ->
    for_ asyncs cancel
