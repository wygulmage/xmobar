{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Main
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The main module of Xmobar, a text based status bar
--
-----------------------------------------------------------------------------

module Main (main) where

import Data.List (intercalate)

import System.Environment (getArgs)
import Control.Monad (unless)
import System.Posix.Files (fileExist)

import Xmobar
import Xmobar.App.Main
import Xmobar.App.Opts

-- $main

-- | The main entry point
main :: IO ()
main = do
  (o,file) <- getArgs >>= getOpts
  (c,defaultings) <- case file of
                       [cfgfile] -> config cfgfile usage
                       _ -> defConfig usage
  unless (null defaultings || notElem Debug o) $ putStrLn $
    "Fields missing from config defaulted: " ++ intercalate "," defaultings
  doOpts c o >>= xmobar

-- | Read default configuration file or load the default config
defConfig :: String -> IO (Config,[String])
defConfig msg = do
  xdgConfigFile <- xmobarConfigFile
  xdgConfigFileExists <- fileExist xdgConfigFile
  if xdgConfigFileExists
    then config xdgConfigFile msg
    else return (defaultConfig,[])

config :: FilePath -> String -> IO (Config,[String])
config f msg = do
  let err m = error $ f ++ ": " ++ m
  file <- fileExist f
  r <- if file
       then readConfig defaultConfig f
       else err $ "file not found" ++ "\n" ++ msg
  case r of
    Left e -> err (show e)
    Right res -> return res
