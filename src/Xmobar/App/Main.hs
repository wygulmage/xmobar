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


module Xmobar.App.Main (xmobar, doOpts) where

import Data.Foldable (for_)
import qualified Data.Map as Map
import System.Exit
import Text.Read (readMaybe)

import Graphics.X11.Xlib
import Control.Concurrent.Async (Async, cancel)
import Control.Exception (bracket)

import Xmobar.Config.Types
import Xmobar.System.Signal (setupSignalHandler, withDeferSignals)
import Xmobar.Run.Template
import Xmobar.X11.Types
import Xmobar.X11.Text
import Xmobar.X11.Window
import Xmobar.App.Opts
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

doOpts :: Config -> [Opts] -> IO Config
doOpts conf [] =
  return (conf {lowerOnStart = lowerOnStart conf && overrideRedirect conf})
doOpts conf (o:oo) =
  case o of
    Help -> putStr   usage >> exitSuccess
    Version -> putStrLn info  >> exitSuccess
    Debug -> doOpts' conf
    Font s -> doOpts' (conf {font = s})
    WmClass s -> doOpts' (conf {wmClass = s})
    WmName s -> doOpts' (conf {wmName = s})
    BgColor s -> doOpts' (conf {bgColor = s})
    FgColor s -> doOpts' (conf {fgColor = s})
    Alpha n -> doOpts' (conf {alpha = read n})
    T -> doOpts' (conf {position = Top})
    B -> doOpts' (conf {position = Bottom})
    D -> doOpts' (conf {overrideRedirect = False})
    AlignSep s -> doOpts' (conf {alignSep = s})
    SepChar s -> doOpts' (conf {sepChar = s})
    Template s -> doOpts' (conf {template = s})
    IconRoot s -> doOpts' (conf {iconRoot = s})
    OnScr n -> doOpts' (conf {position = OnScreen (read n) $ position conf})
    Commands s -> case readCom 'c' s of
                    Right x -> doOpts' (conf {commands = x})
                    Left e -> putStr (e ++ usage) >> exitWith (ExitFailure 1)
    AddCommand s -> case readCom 'C' s of
                      Right x -> doOpts' (conf {commands = commands conf ++ x})
                      Left e -> putStr (e ++ usage) >> exitWith (ExitFailure 1)
    Position s -> readPosition s
  where readCom c str =
          case readStr str of
            [x] -> Right x
            _  -> Left ("xmobar: cannot read list of commands " ++
                        "specified with the -" ++ c:" option\n")
        readStr str = [x | (x,t) <- reads str, ("","") <- lex t]
        doOpts' c = doOpts c oo
        readPosition string =
            case readMaybe string of
                Just x  -> doOpts' (conf { position = x })
                Nothing -> do
                    putStrLn "Can't parse position option, ignoring"
                    doOpts' conf
