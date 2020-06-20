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
import qualified Data.ByteString.Lazy.Char8 as B
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

barField :: String
barField = "bar"

vbarField :: String
vbarField = "vbar"

ipatField :: String
ipatField = "ipat"

totalField :: String
totalField = "total"

userField :: String
userField = "user"

niceField :: String
niceField = "nice"

systemField :: String
systemField = "system"

idleField :: String
idleField = "idle"

iowaitField :: String
iowaitField = "iowait"

cpuConfig :: IO MConfig
cpuConfig =
  mkMConfig
    "Cpu: <total>%"
    [ barField
    , vbarField
    , ipatField
    , totalField
    , userField
    , niceField
    , systemField
    , idleField
    , iowaitField
    ]

type CpuDataRef = IORef [Int]

-- Details about the fields here: https://www.kernel.org/doc/Documentation/filesystems/proc.txt
cpuData :: IO [Int]
cpuData = cpuParser <$> B.readFile "/proc/stat"

readInt :: B.ByteString -> Int
readInt bs = case B.readInt bs of
               Nothing -> 0
               Just (i, _) -> i

cpuParser :: B.ByteString -> [Int]
cpuParser = map readInt . tail . B.words . head . B.lines

data CpuData = CpuData {
      cpuUser :: !Float,
      cpuNice :: !Float,
      cpuSystem :: !Float,
      cpuIdle :: !Float,
      cpuIowait :: !Float,
      cpuTotal :: !Float
    }

convertToCpuData :: [Float] -> CpuData
convertToCpuData (u:n:s:ie:iw:_) =
  CpuData
    { cpuUser = u
    , cpuNice = n
    , cpuSystem = s
    , cpuIdle = ie
    , cpuIowait = iw
    , cpuTotal = sum [u, n, s]
    }
convertToCpuData args = error $ "convertToCpuData: Unexpected list" <> show args

parseCpu :: CpuDataRef -> IO CpuData
parseCpu cref =
    do a <- readIORef cref
       b <- cpuData
       writeIORef cref b
       let dif = zipWith (-) b a
           tot = fromIntegral $ sum dif
           safeDiv n = case tot of
                         0 -> 0
                         v -> fromIntegral n / v
           percent = map safeDiv dif
       return $ convertToCpuData percent

data Field = Field {
      fieldName :: !String,
      fieldCompute :: !ShouldCompute
    } deriving (Eq, Ord, Show)

data ShouldCompute = Compute | Skip deriving (Eq, Ord, Show)

formatField :: MonitorConfig -> CpuOpts -> CpuData -> Field -> IO String
formatField cpuParams cpuOpts cpuInfo@CpuData {..} Field {..}
  | fieldName == barField =
    if fieldCompute == Compute
      then pShowPercentBar cpuParams (100 * cpuTotal) cpuTotal
      else pure []
  | fieldName == vbarField =
    if fieldCompute == Compute
      then pShowVerticalBar cpuParams (100 * cpuTotal) cpuTotal
      else pure []
  | fieldName == ipatField =
    if fieldCompute == Compute
      then pShowIconPattern (loadIconPattern cpuOpts) cpuTotal
      else pure []
  | otherwise =
    if fieldCompute == Compute
      then pShowPercentWithColors cpuParams (getFieldValue fieldName cpuInfo)
      else pure []

getFieldValue :: String -> CpuData -> Float
getFieldValue field CpuData{..}
    | field == barField = cpuTotal
    | field == vbarField = cpuTotal
    | field == ipatField = cpuTotal
    | field == totalField = cpuTotal
    | field == userField = cpuUser
    | field == niceField = cpuNice
    | field == systemField = cpuSystem
    | field == idleField = cpuIdle
    | otherwise = cpuIowait

computeFields :: [String] -> [String] -> [Field]
computeFields [] _ = []
computeFields (x:xs) inputFields =
  if x `elem` inputFields
    then (Field {fieldName = x, fieldCompute = Compute}) :
         computeFields xs inputFields
    else (Field {fieldName = x, fieldCompute = Skip}) :
         computeFields xs inputFields

formatCpu :: CpuArguments -> CpuData -> IO [String]
formatCpu CpuArguments{..} cpuInfo = do
  strs <- mapM (formatField cpuParams cpuOpts cpuInfo) cpuFields
  pure $ filter (not . null) strs

getInputFields :: CpuArguments -> [String]
getInputFields CpuArguments{..} = map (\(_,f,_) -> f) cpuInputTemplate

optimizeAllTemplate :: CpuArguments -> CpuArguments
optimizeAllTemplate args@CpuArguments {..} =
  let inputFields = getInputFields args
      allTemplates =
        filter (\(field, _) -> field `elem` inputFields) cpuAllTemplate
   in args {cpuAllTemplate = allTemplates}

data CpuArguments =
  CpuArguments
    { cpuDataRef :: !CpuDataRef
    , cpuParams :: !MonitorConfig
    , cpuArgs :: ![String]
    , cpuOpts :: !CpuOpts
    , cpuInputTemplate :: ![(String, String, String)] -- [("Cpu: ","total","% "),("","user","%")]
    , cpuAllTemplate :: ![(String, [(String, String, String)])] -- [("bar",[]),("vbar",[]),("ipat",[]),("total",[]),...]
    , cpuFields :: ![Field]
    }


getArguments :: [String] -> IO CpuArguments
getArguments cpuArgs = do
  initCpuData <- cpuData
  cpuDataRef <- newIORef initCpuData
  void $ parseCpu cpuDataRef
  cpuParams <- computeMonitorConfig cpuArgs cpuConfig
  cpuInputTemplate <- runTemplateParser cpuParams
  cpuAllTemplate <- runExportParser (pExport cpuParams)
  nonOptions <-
    case getOpt Permute pluginOptions cpuArgs of
      (_, n, []) -> pure n
      (_, _, errs) -> error $ "getArguments: " <> show errs
  cpuOpts <-
    case getOpt Permute options nonOptions of
      (o, _, []) -> pure $ foldr id defaultOpts o
      (_, _, errs) -> error $ "getArguments options: " <> show errs
  let cpuFields =
        computeFields
          (map fst cpuAllTemplate)
          (map (\(_, f, _) -> f) cpuInputTemplate)
  pure $ optimizeAllTemplate CpuArguments {..}


runCpu :: CpuArguments -> IO String
runCpu args@CpuArguments {..} = do
  cpuValue <- parseCpu cpuDataRef
  temMonitorValues <- formatCpu args cpuValue
  let templateInput =
        TemplateInput
          { temInputTemplate = cpuInputTemplate
          , temAllTemplate = cpuAllTemplate
          , ..
          }
  pureParseTemplate cpuParams templateInput

startCpu :: [String] -> Int -> (String -> IO ()) -> IO ()
startCpu args refreshRate cb = do
  cpuArgs <- getArguments args
  doEveryTenthSeconds refreshRate (runCpu cpuArgs >>= cb)
