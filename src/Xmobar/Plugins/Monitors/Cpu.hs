{-#LANGUAGE RecordWildCards#-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Cpu
-- Copyright   :  (c) 2011, 2017 Jose Antonio Ortega Ruiz
--                (c) 2007-2010 Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A cpu monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Cpu
  ( startCpu
  , runCpu
  , cpuConfig
  , CpuDataRef
  , CpuOpts
  , CpuArguments
  , parseCpu
  , getArguments
  ) where

import Xmobar.Plugins.Monitors.Common
import qualified Data.ByteString.Char8 as B
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Console.GetOpt
import Xmobar.App.Timer (doEveryTenthSeconds)
import Control.Monad (void)

newtype CpuOpts = CpuOpts
  { loadIconPattern :: Maybe IconPattern
  }

defaultOpts :: CpuOpts
defaultOpts = CpuOpts
  { loadIconPattern = Nothing
  }

options :: [OptDescr (CpuOpts -> CpuOpts)]
options =
  [ Option "" ["load-icon-pattern"] (ReqArg (\x o ->
     o { loadIconPattern = Just $ parseIconPattern x }) "") ""
  ]

cpuConfig :: IO MConfig
cpuConfig = mkMConfig
       "Cpu: <total>%"
       ["bar","vbar","ipat","total","user","nice","system","idle","iowait"]

type CpuDataRef = IORef [Int]

cpuData :: IO [Int]
cpuData = cpuParser <$> B.readFile "/proc/stat"

readInt :: B.ByteString -> Int
readInt bs = case B.readInt bs of
               Nothing -> 0
               Just (i, _) -> i

cpuParser :: B.ByteString -> [Int]
cpuParser = map readInt . tail . B.words . head . B.lines

parseCpu :: CpuDataRef -> IO [Float]
parseCpu cref =
    do a <- readIORef cref
       b <- cpuData
       writeIORef cref b
       let dif = zipWith (-) b a
           tot = fromIntegral $ sum dif
           percent = map ((/ tot) . fromIntegral) dif
       return percent

formatCpu :: CpuOpts -> [Float] -> PureConfig -> IO [String]
formatCpu _ [] _ = return $ replicate 8 ""
formatCpu opts xs p = do
  let t = sum $ take 3 xs
  b <- pShowPercentBar p (100 * t) t
  v <- pShowVerticalBar p (100 * t) t
  d <- pShowIconPattern (loadIconPattern opts) t
  ps <- pShowPercentsWithColors p (t:xs)
  return $ (b:v:d:ps)

data CpuArguments = CpuArguments {
      cpuDataRef :: !CpuDataRef,
      cpuParams :: !PureConfig,
      cpuArgs :: ![String],
      cpuOpts :: !CpuOpts,
      cpuInputTemplate :: ![(String, String, String)], -- [("Cpu: ","total","% "),("","user","%")]
      cpuAllTemplate :: ![(String, [(String, String, String)])] -- [("bar",[]),("vbar",[]),("ipat",[]),("total",[]),...]
    }

getArguments :: [String] -> IO CpuArguments
getArguments cpuArgs = do
  cpuDataRef <- newIORef []
  cpuParams <- computePureConfig cpuArgs cpuConfig
  cpuInputTemplate <- runTemplateParser cpuParams
  cpuAllTemplate <- runExportParser (pExport cpuParams)
  nonOptions <- case getOpt Permute commonOptions cpuArgs of
                  (_, n, []) -> pure n
                  (_,_,errs) -> error $ "getArguments: " <> show errs
  cpuOpts <- case getOpt Permute options nonOptions of
                  (o, _, []) -> pure $ foldr id defaultOpts o
                  (_,_,errs) -> error $ "getArguments options: " <> show errs
  pure CpuArguments{..}


runCpu :: CpuArguments -> IO String
runCpu CpuArguments{..} = do
  cpuValue <- parseCpu cpuDataRef
  temMonitorValues <- formatCpu cpuOpts cpuValue cpuParams
  let templateInput = TemplateInput { temInputTemplate = cpuInputTemplate, temAllTemplate = cpuAllTemplate, ..}
  pureParseTemplate cpuParams templateInput

startCpu :: [String] -> Int -> (String -> IO ()) -> IO ()
startCpu args refreshRate cb = do
  cref <- newIORef []
  void $ parseCpu cref
  cpuArgs <- getArguments args
  doEveryTenthSeconds refreshRate (runCpu cpuArgs >>= cb)
