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

data CpuArguments = CpuArguments {
      cpuRef :: CpuDataRef,
      cpuMConfig :: MConfig,
      cpuArgs :: [String]
    }

mkCpuArgs :: IO CpuArguments
mkCpuArgs = do
  cpuRef <- newIORef []
  _ <- parseCpu cpuRef
  cpuMConfig <- cpuConfig
  let cpuArgs = ["-L","3","-H","50","--normal","green","--high","red"]
  pure $ CpuArguments {..}

-- | The action which will be benchmarked
cpuAction :: CpuArguments -> IO String
cpuAction CpuArguments{..} = runMonitor cpuMConfig (doArgs cpuArgs (runCpu cpuRef) (\_ -> return True))


cpuBenchmark :: CpuArguments -> Benchmarkable
cpuBenchmark cpuParams = nfIO $ cpuAction cpuParams

normalCpuBench :: CpuArguments -> [Benchmark]
normalCpuBench args = [bench "CPU normal args" (cpuBenchmark args)]
