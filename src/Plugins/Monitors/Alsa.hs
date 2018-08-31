-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Alsa
-- Copyright   :  (c) 2018 Daniel Schüssler
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Event-based variant of the Volume plugin.
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Alsa
  ( startAlsaPlugin
  , withMonitorWaiter
  , parseOptsIncludingMonitorArgs
  , AlsaOpts(aoAlsaCtlPath)
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Plugins.Monitors.Common
import Plugins.Monitors.Volume(volumeConfig, VolumeOpts, runVolumeWith)
import qualified Plugins.Monitors.Volume as Volume;
import System.Console.GetOpt
import System.Directory
import System.Exit
import System.IO
import System.Process

data AlsaOpts = AlsaOpts
    { aoVolumeOpts :: VolumeOpts
    , aoAlsaCtlPath :: Maybe FilePath
    }

defaultOpts :: AlsaOpts
defaultOpts = AlsaOpts Volume.defaultOpts Nothing

alsaCtlOptionName :: String
alsaCtlOptionName = "alsactl"

options :: [OptDescr (AlsaOpts -> AlsaOpts)]
options =
    Option "" [alsaCtlOptionName] (ReqArg (\x o ->
       o { aoAlsaCtlPath = Just x }) "") ""
    : fmap (fmap modifyVolumeOpts) Volume.options
  where
    modifyVolumeOpts f o = o { aoVolumeOpts = f (aoVolumeOpts o) }

parseOpts :: [String] -> IO AlsaOpts
parseOpts argv =
    case getOpt Permute options argv of
        (o, _, []) -> return $ foldr id defaultOpts o
        (_, _, errs) -> ioError . userError $ concat errs

parseOptsIncludingMonitorArgs :: [String] -> IO AlsaOpts
parseOptsIncludingMonitorArgs args =
    -- Drop generic Monitor args first
    case getOpt Permute [] args of
      (_, args', _) -> parseOpts args'

startAlsaPlugin :: String -> String -> [String] -> (String -> IO ()) -> IO ()
startAlsaPlugin mixerName controlName args cb = do
  opts <- parseOptsIncludingMonitorArgs args

  let run args2 = do
        -- Replicating the reparsing logic used by other plugins for now,
        -- but it seems the option parsing could be floated out (actually,
        -- GHC could in principle do it already since getOpt is pure, but
        -- it would have to inline 'runMBD', 'doArgs' and 'parseOpts' to see
        -- it, which probably isn't going to happen with the default
        -- optimization settings).
        opts2 <- io $ parseOpts args2
        runVolumeWith (aoVolumeOpts opts2) mixerName controlName

  withMonitorWaiter mixerName (aoAlsaCtlPath opts) $ \wait_ ->
    runMB args volumeConfig run wait_ cb

withMonitorWaiter :: String -> Maybe FilePath -> (IO () -> IO a) -> IO a
withMonitorWaiter mixerName alsaCtlPath cont = do
  mvar <- newMVar ()

  path <- determineAlsaCtlPath

  bracket (async $ readerThread mvar path) cancel $ \a -> do

    -- Throw on this thread if there's an exception
    -- on the reader thread.
    link a

    cont $ takeMVar mvar

  where

    readerThread mvar path =
      let createProc = (proc "stdbuf" ["-oL", path, "monitor", mixerName])
                          {std_out = CreatePipe}
      in
        withCreateProcess createProc $ \_ (Just alsaOut) _ _ -> do
          hSetBuffering alsaOut LineBuffering

          forever $ do
            c <- hGetChar alsaOut
            when (c == '\n') $
              -- This uses 'tryPutMVar' because 'putMVar' would make 'runVolume' run
              -- once for each event. But we want it to run only once after a burst
              -- of events.
              void $ tryPutMVar mvar ()

    defaultPath = "/usr/sbin/alsactl"

    determineAlsaCtlPath =
      case alsaCtlPath of
        Just path -> do
          found <- doesFileExist path
          if found
            then pure path
            else throwIO . ErrorCall $
                  "Specified alsactl file " ++ path ++ " does not exist"

        Nothing -> do
          (ec, path, err) <- readProcessWithExitCode "which" ["alsactl"] ""
          unless (null err) $ hPutStrLn stderr err
          case ec of
            ExitSuccess -> pure $ trimTrailingNewline path
            ExitFailure _ -> do
              found <- doesFileExist defaultPath
              if found
                then pure defaultPath
                else throwIO . ErrorCall $
                      "alsactl not found in PATH or at " ++
                      show defaultPath ++
                      "; please specify with --" ++
                      alsaCtlOptionName ++ "=/path/to/alsactl"


-- This is necessarily very inefficient on 'String's
trimTrailingNewline :: String -> String
trimTrailingNewline x =
  case reverse x of
    '\n' : '\r' : y -> reverse y
    '\n' : y -> reverse y
    _ -> x
