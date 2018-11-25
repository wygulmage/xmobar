{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar
-- Copyright   :  (c) 2011, 2012, 2013, 2014, 2015, 2017, 2018 Jose Antonio Ortega Ruiz
--                (c) 2007 Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A status bar for the Xmonad Window Manager
--
-----------------------------------------------------------------------------

module Xmobar (xmobar
              , Runnable (..)
              , module Xmobar.Config
              , module Xmobar.Plugins.BufferedPipeReader
              , module Xmobar.Plugins.CommandReader
              , module Xmobar.Plugins.Date
#ifdef DATEZONE
              , module Xmobar.Plugins.DateZone
#endif
              , module Xmobar.Plugins.EWMH
              , module Xmobar.Plugins.Kbd
              , module Xmobar.Plugins.Locks
#ifdef INOTIFY
              , module Xmobar.Plugins.Mail
              , module Xmobar.Plugins.MBox
#endif
              , module Xmobar.Plugins.Monitors
              , module Xmobar.Plugins.PipeReader
              , module Xmobar.Plugins.StdinReader
              , module Xmobar.Plugins.XMonadLog
              ) where

import Data.Foldable (for_)
import qualified Data.Map as Map

import Graphics.X11.Xlib
import Control.Concurrent.Async (Async, cancel)
import Control.Exception (bracket)

import Xmobar.Config
import Xmobar.Run.Runnable
import Xmobar.Run.Template
import Xmobar.Run.EventLoop (startLoop, startCommand)
import Xmobar.System.Signal (setupSignalHandler, withDeferSignals)
import Xmobar.X11.Types
import Xmobar.X11.XUtil
import Xmobar.X11.Window
import Xmobar.Plugins.BufferedPipeReader
import Xmobar.Plugins.CommandReader
import Xmobar.Plugins.Date
#ifdef DATEZONE
import Xmobar.Plugins.DateZone
#endif
import Xmobar.Plugins.EWMH
import Xmobar.Plugins.Kbd
import Xmobar.Plugins.Locks
#ifdef INOTIFY
import Xmobar.Plugins.Mail
import Xmobar.Plugins.MBox
#endif
import Xmobar.Plugins.Monitors
import Xmobar.Plugins.PipeReader
import Xmobar.Plugins.StdinReader
import Xmobar.Plugins.XMonadLog


splitTemplate :: Config -> [String]
splitTemplate conf =
  case break (==l) t of
    (le,_:re) -> case break (==r) re of
                   (ce,_:ri) -> [le, ce, ri]
                   _         -> def
    _         -> def
  where [l, r] = alignSep
                   (if length (alignSep conf) == 2 then conf else defaultConfig)
        t = template conf
        def = [t, "", ""]

xmobar :: Config -> IO ()
xmobar conf = withDeferSignals $ do
  initThreads
  d <- openDisplay ""
  fs    <- initFont d (font conf)
  fl    <- mapM (initFont d) (additionalFonts conf)
  cls   <- mapM (parseCommands conf) (splitTemplate conf)
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
