-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Commands
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The 'Exec' class and the 'Command' data type.
--
-- The 'Exec' class rappresents the executable types, whose constructors may
-- appear in the 'Config.commands' field of the 'Config.Config' data type.
--
-- The 'Command' data type is for OS commands to be run by xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Run.Commands (Command (..), Exec (..), tenthSeconds) where

import Prelude
import Control.Exception (handle, SomeException(..))
import Data.Char
import System.Process
import System.Exit
import System.IO (hClose)
import Control.Concurrent

import Xmobar.System.Signal
import Xmobar.System.Utils (hGetLineSafe)

-- | Work around to the Int max bound: since threadDelay takes an Int, it
-- is not possible to set a thread delay grater than about 45 minutes.
-- With a little recursion we solve the problem.
tenthSeconds :: Int -> IO ()
tenthSeconds s | s >= x = do threadDelay (x * 100000)
                             tenthSeconds (s - x)
               | otherwise = threadDelay (s * 100000)
               where x = (maxBound :: Int) `div` 100000

class Show e => Exec e where
    alias   :: e -> String
    alias   e    = takeWhile (not . isSpace) $ show e
    rate    :: e -> Int
    rate    _    = 10
    run     :: e -> IO String
    run     _    = return ""
    start   :: e -> (String -> IO ()) -> IO ()
    start   e cb = go
        where go = run e >>= cb >> tenthSeconds (rate e) >> go
    trigger :: e -> (Maybe SignalType -> IO ()) -> IO ()
    trigger _ sh  = sh Nothing

data Command = Com Program Args Alias Rate
             | ComX Program Args String Alias Rate
               deriving (Show,Read,Eq)

type Args    = [String]
type Program = String
type Alias   = String
type Rate    = Int

instance Exec Command where
    alias (ComX p _ _ a _) =
      if p /= "" then (if a == "" then p else a) else ""
    alias (Com p a al r) = alias (ComX p a "" al r)
    start (Com p as al r) cb =
      start (ComX p as ("Could not execute command " ++ p) al r) cb
    start (ComX prog args msg _ r) cb = if r > 0 then go else exec
        where go = exec >> tenthSeconds r >> go
              exec = do
                (i,o,e,p) <- runInteractiveProcess prog args Nothing Nothing
                exit <- waitForProcess p
                let closeHandles = hClose o >> hClose i >> hClose e
                    getL = handle (\(SomeException _) -> return "")
                                  (hGetLineSafe o)
                case exit of
                  ExitSuccess -> do str <- getL
                                    closeHandles
                                    cb str
                  _ -> closeHandles >> cb msg
