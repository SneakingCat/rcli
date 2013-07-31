module System.Console.RemoteCLI.CommandHandler where

import System.Console.RemoteCLI.CommandLine
import System.Console.RemoteCLI.CommandState

-- | Apply the pure handler for the given command line
applyPureHandler :: PureCommandHandler
applyPureHandler _ _ = Left "Command not found"