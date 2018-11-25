{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Mail
-- Copyright   :  (c) Spencer Janssen
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <sjanssen@cse.unl.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for checking mail.
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Mail where

import Xmobar.Plugins
#ifdef INOTIFY
import Xmobar.Utils (expandHome, changeLoop)

import Control.Monad
import Control.Concurrent.STM

import System.Directory
import System.FilePath
import System.INotify

import Data.List (isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as S

#if MIN_VERSION_hinotify(0,3,10)
import qualified Data.ByteString.Char8 as BS (ByteString, pack, unpack)
unpack :: BS.ByteString -> String
unpack = BS.unpack
pack :: String -> BS.ByteString
pack = BS.pack
#else
unpack :: String -> String
unpack = id
pack :: String -> String
pack = id
#endif
#else
import System.IO
#endif


-- | A list of mail box names and paths to maildirs.
data Mail = Mail [(String, FilePath)] String
    deriving (Read, Show)

instance Exec Mail where
    alias (Mail _ a) = a
#ifndef INOTIFY
    start _ _ =
        hPutStrLn stderr $ "Warning: xmobar is not compiled with -fwith_inotify,"
                        ++ " but the Mail plugin requires it."
#else
    start (Mail ms _) cb = do
        vs <- mapM (const $ newTVarIO S.empty) ms

        let ts = map fst ms
            rs = map ((</> "new") . snd) ms
            ev = [Move, MoveIn, MoveOut, Create, Delete]

        ds <- mapM expandHome rs
        i <- initINotify
        zipWithM_ (\d v -> addWatch i ev d (handle v)) (map pack ds) vs

        forM_ (zip ds vs) $ \(d, v) -> do
            s <- fmap (S.fromList . filter (not . isPrefixOf "."))
                    $ getDirectoryContents d
            atomically $ modifyTVar v (S.union s)

        changeLoop (mapM (fmap S.size . readTVar) vs) $ \ns ->
            cb . unwords $ [m ++ show n
                            | (m, n) <- zip ts ns
                            , n /= 0 ]

handle :: TVar (Set String) -> Event -> IO ()
handle v e = atomically $ modifyTVar v $ case e of
    Created  {} -> create
    MovedIn  {} -> create
    Deleted  {} -> delete
    MovedOut {} -> delete
    _           -> id
 where
    delete = S.delete ((unpack . filePath) e)
    create = S.insert ((unpack . filePath) e)
#endif
