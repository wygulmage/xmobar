-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Batt
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A battery monitor for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.Batt where

import qualified Data.ByteString.Lazy.Char8 as B
import Plugins.Monitors.Common
import System.Posix.Files (fileExist)

data Batt = Batt Float String
          | NA

battConfig :: IO MConfig
battConfig = mkMConfig
       "Batt: <left>" -- template
       ["left","status"] -- available replacements

file2batfile :: String -> (String, String, String)
file2batfile s = ( s' ++ "/charge_full"
                 , s' ++ "/charge_now"
                 , s' ++ "/status")
    where s' = "/sys/class/power_supply/" ++ s

readFileBatt :: (String, String, String) -> IO (String, String, String)
readFileBatt (f,n,s) =
    do a <- rf f
       b <- rf n
       c <- rf s
       return (a,b,c)
    where rf file = do
            fe <- fileExist file
            if fe then B.unpack `fmap` catRead file else return []

parseBATT :: [(String, String, String)] -> IO Batt
parseBATT bfs =
    do [(a0,b0,c0),(a1,b1,_),(a2,b2,_)] <- mapM readFileBatt (take 3 $ bfs ++ repeat ("","",""))
       let read' s = if s == [] then 0 else read s
           left    = (read' b0 + read' b1 + read' b2) / (read' a0 + read' a1 + read' a2) --present / full
       return $ if isNaN left then NA else Batt left c0

formatBatt :: Float -> Monitor [String]
formatBatt x =
    do let f s = floatToPercent (s / 100)
       l <- showWithColors f (x * 100)
       return [l]

runBatt :: [String] -> Monitor String
runBatt = runBatt' ["BAT0","BAT1","BAT2"]

runBatt' :: [String] -> [String] -> Monitor String
runBatt' bfs _ = do
  c <- io $ parseBATT (map file2batfile bfs)
  case c of
    Batt x s -> do l <- formatBatt x
                   parseTemplate (l ++ [s])
    NA -> return "N/A"
