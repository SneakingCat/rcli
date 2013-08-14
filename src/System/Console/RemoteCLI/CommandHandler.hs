module System.Console.RemoteCLI.CommandHandler (  
  localHandlers
  ) where

import System.Console.RemoteCLI.CommandLine (CommandLine (..)
                                            , Option (..))
import System.Console.RemoteCLI.CommandState (Synopsis
                                              , PureCommandHandler
                                              , MonadicCommandHandler
                                              , localCommands
                                              , remoteCommands)
import Text.Printf (printf)
      
-- | Export all local handlers defined in this module
localHandlers :: [(String, (Synopsis, PureCommandHandler))]
localHandlers = [("help", ("a synopsis", pureHelpHandler))]

-- | The command handler for the "help" command
pureHelpHandler :: PureCommandHandler
pureHelpHandler commandLine state
  | nopts commandLine > 1   = Left errTooManyOpts
  | nopts commandLine == 1
    && optArg commandLine   = Left errHasOptArg
  | otherwise               = Right (printout, state, monadicDoNothingHandler)
  where
    errTooManyOpts = "Error: Too many options":["Usage: help <COMMAND>"]
    errHasOptArg   = ["Error: Help option cannot have argument"]
    printout       = header:(map toLine $ localCommands state)
                     ++ (map toLine $ remoteCommands state)
    header         = "The available commands are:"
    toLine (cmd, (synopsis, _))    = printf "%-20s%s" cmd synopsis
    nopts (CommandLine _ _ opts)   = length opts
    optArg (CommandLine _ _ opts)  =
      case head opts of
        (Option _ (Just _)) -> True
        _                   -> False

-- | A monadic command handler that return the empty printout and the
-- same state as given as input
monadicDoNothingHandler :: MonadicCommandHandler
monadicDoNothingHandler state = return (Right ([], state))