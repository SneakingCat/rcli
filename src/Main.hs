module Main where

import Control.Monad.MonadCLI (runCLI)
import System.Console.RemoteCLI (dummyEvalLoop)

main :: IO ()
main = runCLI dummyEvalLoop "% "