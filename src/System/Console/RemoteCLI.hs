module System.Console.RemoteCLI (
  dummyEvalLoop
  ) where

import Control.Monad.MonadCLI (CLI, get)
import Control.Monad.IO.Class (liftIO)
import System.Console.Readline (readline, addHistory)

dummyEvalLoop :: CLI String ()
dummyEvalLoop = do
  prompt <- get
  maybeLine <- liftIO $ readline prompt
  case maybeLine of
    Just line -> do liftIO $ addHistory line
                    dummyEvalLoop
    Nothing   -> return ()

