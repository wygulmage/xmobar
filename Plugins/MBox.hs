-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.MBox
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for checking mail in mbox files.
--
-----------------------------------------------------------------------------

module Plugins.MBox (MBox(..)) where

import Prelude hiding (catch)
import System.IO
import Plugins

import Control.Monad
import Control.Concurrent.STM

import System.Directory
import System.FilePath
import System.INotify

import Data.List (isPrefixOf)

-- | A list of display names, paths to mbox files and display colours,
-- followed by a directory to resolve relative path names (can be "")
data MBox = MBox [(String, FilePath, String)] FilePath
          deriving (Read, Show)

instance Exec MBox where
  start (MBox ms dir) cb = do
    vs <- mapM (const $ newTVarIO ("", 0 :: Int)) ms

    dirExists <- doesDirectoryExist dir
    let ts = map (\(t, _, _) -> t) ms
        sec = \(_, f, _) -> f
        md = if dirExists then (dir </>) . sec else sec
        fs = map md ms
        cs = map (\(_, _, c) -> c) ms
        ev = [Modify, Create]

    i <- initINotify
    zipWithM_ (\f v -> addWatch i ev f (handle v)) fs vs

    forM (zip fs vs) $ \(f, v) -> do
      exists <- doesFileExist f
      n <- if exists then countMails f else return 0
      atomically $ writeTVar v (f, n)

    changeLoop (mapM (fmap snd . readTVar) vs) $ \ns ->
      cb . unwords $ [ showC m n c | (m, n, c) <- zip3 ts ns cs, n /= 0 ]

showC :: String -> Int -> String -> String
showC m n c =
  if c == "" then msg else "<fc=" ++ c ++ ">" ++ msg ++ "</fc>"
    where msg = m ++ show n

countMails :: FilePath -> IO Int
countMails f = do
  txt <- readFileSafe f
  return $ length . filter (isPrefixOf "From ") . lines $ txt

handle :: TVar (FilePath, Int) -> Event -> IO ()
handle v _ =  do
  (p, _) <- atomically $ readTVar v
  n <- countMails p
  atomically $ writeTVar v (p, n)

changeLoop :: Eq a => STM a -> (a -> IO ()) -> IO ()
changeLoop s f = atomically s >>= go
 where
    go old = do
        f old
        go =<< atomically (do
            new <- s
            guard (new /= old)
            return new)
