module System.Console.RemoteCLI.Executor (
  evalLoop
  ) where

import System.Console.RemoteCLI.CommandLine
import System.Console.RemoteCLI.CommandState
import System.Console.RemoteCLI.CommandHandler
import Control.Monad.MonadCLI (CLI, get, put)
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
    Just line 
      | null line -> evalLoop -- Empty line, just quit the rest
      | otherwise -> do
        liftIO $ addHistory line
        case (applyPureHandler line state) of
          Left err -> out err
          Right (state', monadicHandler) -> do
            result' <- liftIO $ monadicHandler state'
            case result' of
              Left err -> out err
              Right state'' ->
                put state''                
  evalLoop
  where
    out = liftIO . putStrLn
  
applyPureHandler :: String -> CommandState ->
                    Either String (CommandState, MonadicCommandHandler)
applyPureHandler line state = do
  commandLine <- fromString line
  pureHandler <- lookupHandler commandLine state
  pureHandler commandLine state

  
