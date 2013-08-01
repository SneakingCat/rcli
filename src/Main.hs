module Main where

import Control.Monad.MonadCLI (runCLI)
import System.Console.RemoteCLI.CommandState (empty)
import System.Console.RemoteCLI.Executor (evalLoop)

main :: IO ()
main = runCLI evalLoop empty