{-# LANGUAGE FlexibleContexts, CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Configuration
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Wed Nov 21, 2018 23:13
--
--
-- Parsing configuration files
--
------------------------------------------------------------------------------


module Configuration (readConfig, readDefaultConfig) where

import Control.Monad.IO.Class (liftIO)

import System.Environment
import System.Posix.Files (fileExist)

import qualified Xmobar as X

-- | Reads the configuration files or quits with an error
readConfig :: FilePath -> String -> IO (X.Config,[String])
readConfig f usage = do
  let err m = error $ f ++ ": " ++ m ++ "\n" ++ usage
  file <- liftIO $ fileExist f
  r <- if file then X.readConfig X.defaultConfig f else err "file not found"
  case r of
    Left e -> err (show e)
    Right res -> return res

-- | Read default configuration file or load the default config
readDefaultConfig :: String -> IO (X.Config,[String])
readDefaultConfig usage = do
  xdgConfigFile <- X.getXdgConfigFile
  xdgConfigFileExists <- liftIO $ fileExist xdgConfigFile
  home <- liftIO $ getEnv "HOME"
  let defaultConfigFile = home ++ "/.xmobarrc"
  defaultConfigFileExists <- liftIO $ fileExist defaultConfigFile
  if xdgConfigFileExists
    then readConfig xdgConfigFile usage
    else if defaultConfigFileExists
         then readConfig defaultConfigFile usage
         else return (X.defaultConfig,[])
