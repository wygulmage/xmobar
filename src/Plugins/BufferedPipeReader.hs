-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.BufferedPipeReader
-- Copyright   :  (c) Jochen Keil
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jochen Keil <jochen dot keil at gmail dot com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin for reading (temporarily) from named pipes with reset
--
-----------------------------------------------------------------------------

module Plugins.BufferedPipeReader where

import Control.Monad(forM_, when)
import Control.Concurrent
import Control.Concurrent.STM
import System.IO
-- import System.IO.Unsafe(unsafePerformIO)

import Plugins

data BufferedPipeReader = BufferedPipeReader String [(Int, String)]
    deriving (Read, Show)

-- pipeState :: MVar String
-- pipeState = unsafePerformIO $ newMVar ""

-- pipe :: (String -> IO ()) -> Handle -> IO ()
-- pipe cb h = hGetLineSafe h >>= cb

instance Exec BufferedPipeReader where
    alias ( BufferedPipeReader a _  )    = a
    start ( BufferedPipeReader _ ps ) cb = do

        (chan, str, rst) <- initV
        forM_ ps $ \p -> forkIO $ reader p chan
        writer chan str rst

        where
        initV :: IO ( TChan (Int, String), TVar String, TVar Bool )
        initV = atomically $ do
            tc <- newTChan
            ts <- newTVar ""
            tb <- newTVar False
            return (tc, ts, tb)

        reader :: (Int, FilePath) -> TChan (Int, String) -> IO ()
        reader p@(to, fp) tc = do
            openFile fp ReadWriteMode >>= hGetLineSafe >>= \dt ->
                atomically $ writeTChan tc (to, dt)
            reader p tc

        writer :: TChan (Int, String) -> TVar String -> TVar Bool -> IO ()
        writer tc ts otb = do
            (to, dt, ntb) <- update
            cb dt
            when (to /= 0) $ sfork $ reset to ts ntb
            writer tc ts ntb

            where
            sfork :: IO () -> IO ()
            sfork f = forkIO f >> return ()

            update :: IO (Int, String, TVar Bool)
            update = atomically $ do
                (to, dt) <- readTChan tc
                when (to == 0) $ writeTVar ts dt
                writeTVar otb False
                tb <- newTVar True
                return (to, dt, tb)

        reset :: Int -> TVar String -> TVar Bool -> IO ()
        reset to ts tb = do
            threadDelay ( to * 100 * 1000 )
            readTVarIO tb >>= flip when ( readTVarIO ts >>= cb )
