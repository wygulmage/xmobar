------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.Timer
-- Copyright: (c) 2019 Tomáš Janoušek
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: Tomáš Janoušek <tomi@nomi.cz>
-- Stability: unstable
--
-- Timer coalescing for recurring actions.
--
------------------------------------------------------------------------------

module Xmobar.App.Timer (doEveryTenthSeconds, withTimer) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception (bracket, bracket_)
import Control.Monad (forever, forM, forM_, guard)
import Data.IORef
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Unique
import System.IO.Unsafe (unsafePerformIO)

newtype Timer = Timer (TVar Periods)

type Periods = Map Unique Period

data Period = Period { rate :: Int64, next :: Int64, tick :: TMVar (TMVar ()) }

{-# NOINLINE timer #-}
timer :: IORef (Maybe Timer)
timer = unsafePerformIO (newIORef Nothing)

now :: IO Int64
now = do
    posix <- getPOSIXTime
    return $ floor (10 * posix)

newPeriod :: Int64 -> IO (Unique, Period)
newPeriod r = do
    u <- newUnique
    t <- now
    v <- atomically newEmptyTMVar
    let t' = t - t `mod` r
    return (u, Period { rate = r, next = t', tick = v })

-- | Perform a given action every N tenths of a second.
--
-- The timer is aligned with other timers to minimize the number of wakeups
-- and unnecessary redraws.
doEveryTenthSeconds :: Int -> IO () -> IO ()
doEveryTenthSeconds r action = do
    Just t <- readIORef timer
    doEveryTenthSeconds' t r action

doEveryTenthSeconds' :: Timer -> Int -> IO () -> IO ()
doEveryTenthSeconds' (Timer periodsVar) r action = do
    (u, p) <- newPeriod (fromIntegral r)
    bracket_ (push u p) (pop u) $ forever $
        bracket (atomically $ takeTMVar $ tick p)
                (\doneVar -> atomically $ putTMVar doneVar ())
                (const action)
    where
        push u p = atomically $ modifyTVar periodsVar (M.insert u p)
        pop u = atomically $ modifyTVar periodsVar (M.delete u)

-- | Start the timer coordination thread.
withTimer :: (IO () -> IO ()) -> IO a -> IO a
withTimer pauseRefresh action = do
    periodsVar <- atomically $ newTVar M.empty
    withAsync (timerLoop pauseRefresh periodsVar) $ \_ ->
        bracket_
            (writeIORef timer (Just (Timer periodsVar)))
            (writeIORef timer Nothing) -- TODO: kill all periods?
            action

timerLoop :: (IO () -> IO ()) -> TVar Periods -> IO ()
timerLoop pauseRefresh periodsVar = forever $ do
    t <- now
    toFire <- atomically $ do
        periods <- readTVar periodsVar
        writeTVar periodsVar (advanceTimers t periods)
        return (timersToFire t periods)
    pauseRefresh $ do
        -- Fire timers ...
        doneVars <- atomically $ forM toFire $ \p -> do
            doneVar <- newEmptyTMVar
            putTMVar (tick p) doneVar
            return doneVar
        -- ... and wait for them to avoid unnecessary redraws.
        atomically $ forM_ doneVars takeTMVar
    delayUntilNextFire periodsVar

advanceTimers :: Int64 -> Periods -> Periods
advanceTimers t = M.map advance
    where
        advance p | next p <= t = p { next = t - t `mod` rate p + rate p }
                  | otherwise = p

timersToFire :: Int64 -> Periods -> [Period]
timersToFire t periods = [ p | p <- M.elems periods, next p <= t ]

nextFireTime :: Periods -> Maybe Int64
nextFireTime periods
    | M.null periods = Nothing
    | otherwise = Just $ minimum [ next p | p <- M.elems periods ]

delayUntilNextFire :: TVar Periods -> IO ()
delayUntilNextFire periodsVar = do
    tMaybeNext <- fmap nextFireTime $ readTVarIO periodsVar
    tNow <- now
    delayVar <- case tMaybeNext of
        Just tNext -> do
            -- Work around the Int max bound: threadDelay takes an Int, we can
            -- only sleep for so long, which is okay, we'll just check timers
            -- sooner and sleep again.
            let maxDelay = (maxBound :: Int) `div` 100000
                delay = (tNext - tNow) `min` fromIntegral maxDelay
                delayUsec = fromIntegral delay * 100000
            registerDelay delayUsec
        Nothing -> atomically $ newTVar False
    atomically $ do
        delayOver <- readTVar delayVar
        tMaybeNext' <- fmap nextFireTime $ readTVar periodsVar
        -- Return also if a new period is added (it may fire sooner).
        guard $ delayOver || tMaybeNext /= tMaybeNext'
