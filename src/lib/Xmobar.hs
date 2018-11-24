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

module Xmobar (xmobar) where

import Xmobar.EventLoop (startLoop, startCommand)
import Xmobar.Config

import Data.Foldable (for_)
import qualified Data.Map as Map

import Graphics.X11.Xlib
import Control.Concurrent.Async (Async, cancel)
import Control.Exception (bracket)

import Xmobar.Parsers
import Xmobar.XUtil
import Xmobar.Config()
import Xmobar.Signal (setupSignalHandler, withDeferSignals)
import Xmobar.Window
import Xmobar.Types

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
  cls   <- mapM (parseTemplate conf) (splitTemplate conf)
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
