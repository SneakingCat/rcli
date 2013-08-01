module System.Console.RemoteCLI.Executor (
  dummyEvalLoop
  ) where

import System.Console.RemoteCLI.CommandLine
import System.Console.RemoteCLI.CommandState
import System.Console.RemoteCLI.CommandHandler
import Control.Monad.MonadCLI (CLI, get)
import Control.Monad.IO.Class (liftIO)
import System.Console.Readline (readline, addHistory)

prompt :: String
prompt = "%> "

evalLoop :: CLI CommandState ()
evalLoop = do
  state <- get
  maybeLine <- liftIO $ readline prompt
  case maybeLine of
    Nothing   -> return () -- Ctrl^D is hit, just terminate
    Just line -> do
      liftIO $ addHistory line
      let result = do
            commandLine <- fromString line
            Right 1
      evalLoop

dummyEvalLoop :: CLI String ()
dummyEvalLoop = do
  prompt <- get
  maybeLine <- liftIO $ readline prompt
  case maybeLine of
    Just line -> do liftIO $ addHistory line
                    dummyEvalLoop
    Nothing   -> return ()

