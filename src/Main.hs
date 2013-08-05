module Main where

import Control.Monad.MonadCLI (runCLI)
import System.Console.RemoteCLI.CommandHandler (localHandlers)
import System.Console.RemoteCLI.CommandState (CommandState (..)
                                              , empty
                                              , fromList)
import System.Console.RemoteCLI.Executor (evalLoop)

main :: IO ()
main = runCLI evalLoop initialState

initialState :: CommandState
initialState = let (CommandState v _ r _) = empty
                   l'                     = fromList localHandlers
               in (CommandState v l' r l')
