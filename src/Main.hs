module Main where

import Control.Monad.MonadCLI (runCLI)
import System.Console.RemoteCLI.Executor (dummyEvalLoop)

main :: IO ()
main = runCLI dummyEvalLoop "% "