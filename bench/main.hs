{-#LANGUAGE RecordWildCards#-}

import Gauge
import Xmobar
import Xmobar.Plugins.Monitors.Common.Types
import Xmobar.Plugins.Monitors.Common.Run
import Xmobar.Plugins.Monitors.Cpu
import Control.Monad.Reader
import Data.IORef (newIORef)

main :: IO ()
main = do
  cpuParams <- mkCpuArgs
  defaultMain $ normalBench cpuParams
    where
      normalBench args = [ bgroup "Cpu Benchmarks" $ normalCpuBench args]

runMonitor :: MConfig -> Monitor a -> IO a
runMonitor config r = runReaderT r config

mkCpuArgs :: IO CpuArguments
mkCpuArgs = getArguments ["-L","3","-H","50","--normal","green","--high","red", "-t", "Cpu: <total>%"]
  
-- | The action which will be benchmarked
cpuAction :: CpuArguments -> IO String
cpuAction = runCpu

cpuBenchmark :: CpuArguments -> Benchmarkable
cpuBenchmark cpuParams = nfIO $ cpuAction cpuParams

normalCpuBench :: CpuArguments -> [Benchmark]
normalCpuBench args = [bench "CPU normal args" (cpuBenchmark args)]
