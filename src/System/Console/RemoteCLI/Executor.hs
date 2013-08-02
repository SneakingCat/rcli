-- | Module which define the read and evaluation loop for the CLI
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

-- | Evaluation loop. Outlines the application flow for the CLI
evalLoop :: CLI CommandState ()
evalLoop = do
  maybeLine <- liftIO $ readline prompt
  case maybeLine of
    -- EOF is found. Terminate the evaluation loop
    Nothing       -> return ()
    Just line       
      | null line -> evalLoop
      | otherwise -> do
        liftIO $ addHistory line
        state <- get
        -- First step is the apply the pure command handler ...
        case applyPureHandler line state of
          Left errPrintout -> display errPrintout
          Right (purePrintout, state', monadicHandler) -> do
            -- ... if it succeeds display its printout and apply the
            -- monadic handler
            display purePrintout
            result <- liftIO $ monadicHandler state'
            case result of
              Left errPrintout -> display errPrintout
              Right (monadicPrintout, nextState) -> do
                -- If the monadic handler succeeds display its
                -- printout and update with the new state. The state
                -- will only be updated if both handler sequences are
                -- successful
                display monadicPrintout
                put nextState
  -- Run next iteration
  evalLoop
  where
    display = liftIO . mapM_ putStrLn
  
-- | Find and apply the pure command handler for the command line. The
-- functions are run as actions within the Either monad
applyPureHandler :: String -> CommandState ->
                    Either Printout (Printout
                                    , CommandState
                                    , MonadicCommandHandler)
applyPureHandler line state = do
  commandLine <- fromString line
  pureHandler <- lookupHandler commandLine state
  pureHandler commandLine state

  
