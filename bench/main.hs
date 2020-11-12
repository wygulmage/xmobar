module Main (main) where

import Gauge
import Xmobar
import Xmobar.Plugins.Monitors.Cpu

main :: IO ()
main = do
  defaultMain =<< sequence [cpuBench]

mkCpuArgs :: IO CpuArguments
mkCpuArgs = getArguments ["-L", "3", "-H", "50", "--normal", "green", "--high", "red", "-t", "Cpu: <total>%"]

cpuBench :: IO Benchmark
cpuBench = do
  cpuArgs <- mkCpuArgs
  return $ bgroup "Cpu Benchmarks"
    [ bench "CPU normal args" $ nfIO (runCpu cpuArgs)
    ]
