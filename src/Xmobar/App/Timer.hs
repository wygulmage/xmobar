{-# LANGUAGE LambdaCase #-}
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

module Xmobar.App.Timer
    ( doEveryTenthSeconds
    , tenthSeconds
    , withTimer
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, forM, guard)
import Data.Foldable (foldrM)
import Data.IORef
import Data.Int (Int64)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Unique
import System.IO.Unsafe (unsafePerformIO)

newtype Timer = Timer (TVar Periods)

type Periods = Map Unique Period

data Tick = Tick (TMVar ()) | TimeOut

data Period = Period { rate :: Int64, next :: Int64, tick :: TMVar Tick }

data TimeOutException = TimeOutException deriving Show
instance Exception TimeOutException

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
-- The timer is aligned (coalesced) with other timers to minimize the number
-- of wakeups and unnecessary redraws. If the action takes too long (one
-- second or when the next timer is due), coalescing is disabled for it and it
-- falls back to periodic sleep.
doEveryTenthSeconds :: Int -> IO () -> IO ()
doEveryTenthSeconds r action = do
    Just t <- readIORef timer
    doEveryTenthSecondsCoalesced t r action `catch` \TimeOutException ->
        doEveryTenthSecondsSleeping r action

-- | Perform a given action every N tenths of a second,
-- coalesce with other timers using a given Timer instance.
doEveryTenthSecondsCoalesced :: Timer -> Int -> IO () -> IO ()
doEveryTenthSecondsCoalesced (Timer periodsVar) r action = do
    (u, p) <- newPeriod (fromIntegral r)
    bracket_ (push u p) (pop u) $ forever $ bracket (wait p) done $ const action
    where
        push u p = atomically $ modifyTVar periodsVar (M.insert u p)
        pop u = atomically $ modifyTVar periodsVar (M.delete u)

        wait p = atomically (takeTMVar $ tick p) >>= \case
            Tick doneVar -> return doneVar
            TimeOut -> throwIO TimeOutException
        done doneVar = atomically $ putTMVar doneVar ()

-- | Perform a given action every N tenths of a second,
-- making no attempt to synchronize with other timers.
doEveryTenthSecondsSleeping :: Int -> IO () -> IO ()
doEveryTenthSecondsSleeping r action = go
    where go = action >> tenthSeconds r >> go

-- | Sleep for a given amount of tenths of a second.
--
-- (Work around the Int max bound: since threadDelay takes an Int, it
-- is not possible to set a thread delay grater than about 45 minutes.
-- With a little recursion we solve the problem.)
tenthSeconds :: Int -> IO ()
tenthSeconds s | s >= x = do threadDelay (x * 100000)
                             tenthSeconds (s - x)
               | otherwise = threadDelay (s * 100000)
               where x = (maxBound :: Int) `div` 100000

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
    tNow <- now
    (toFire, tMaybeNext) <- atomically $ do
        periods <- readTVar periodsVar
        let toFire = timersToFire tNow periods
        let periods' = advanceTimers tNow periods
        let tMaybeNext = nextFireTime periods'
        writeTVar periodsVar periods'
        return (toFire, tMaybeNext)
    pauseRefresh $ do
        -- To avoid multiple refreshes, pause refreshing for up to 1 second,
        -- fire timers and wait for them to finish (update their text).
        -- Those that need more time (e.g. weather monitors) will be dropped
        -- from timer coalescing and will fall back to periodic sleep.
        timeoutVar <- registerDelay $ case tMaybeNext of
            Just tNext -> fromIntegral ((tNext - tNow) `max` 10) * 100000
            Nothing -> 1000000
        fired <- fireTimers toFire
        timeouted <- waitForTimers timeoutVar fired
        timeoutTimers timeouted periodsVar
    delayUntilNextFire periodsVar

advanceTimers :: Int64 -> Periods -> Periods
advanceTimers t = M.map advance
    where
        advance p | next p <= t = p { next = t - t `mod` rate p + rate p }
                  | otherwise = p

timersToFire :: Int64 -> Periods -> [(Unique, Period)]
timersToFire t periods = [ (u, p) | (u, p) <- M.toList periods, next p <= t ]

nextFireTime :: Periods -> Maybe Int64
nextFireTime periods
    | M.null periods = Nothing
    | otherwise = Just $ minimum [ next p | p <- M.elems periods ]

fireTimers :: [(Unique, Period)] -> IO [(Unique, TMVar ())]
fireTimers toFire = atomically $ forM toFire $ \(u, p) -> do
    doneVar <- newEmptyTMVar
    putTMVar (tick p) (Tick doneVar)
    return (u, doneVar)

waitForTimers :: TVar Bool -> [(Unique, TMVar ())] -> IO [Unique]
waitForTimers timeoutVar fired = atomically $ do
    timeoutOver <- readTVar timeoutVar
    dones <- forM fired $ \(u, doneVar) -> do
        done <- isJust <$> tryReadTMVar doneVar
        return (u, done)
    guard $ timeoutOver || all snd dones
    return [u | (u, False) <- dones]

-- | Handle slow timers (drop and signal them to stop coalescing).
timeoutTimers :: [Unique] -> TVar Periods -> IO ()
timeoutTimers timers periodsVar = atomically $ do
    periods <- readTVar periodsVar
    periods' <- foldrM timeoutTimer periods timers
    writeTVar periodsVar periods'

timeoutTimer :: Unique -> Periods -> STM Periods
timeoutTimer u periods = do
    putTMVar (tick (periods M.! u)) TimeOut
    return $ u `M.delete` periods

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
