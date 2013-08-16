module System.Console.RemoteCLI.CommandHandler (  
  localHandlers
  ) where

import System.Console.RemoteCLI.CommandLine (CommandLine (..)
                                            , Option (..))
import System.Console.RemoteCLI.CommandState (Synopsis
                                              , Help
                                              , PureCommandHandler
                                              , MonadicCommandHandler
                                              , localCommands
                                              , remoteCommands)
import Text.Printf (printf)
      
-- | Export all local handlers defined in this module
localHandlers :: [(String, (Synopsis, Help, PureCommandHandler))]
localHandlers = [("help", ("a synopsis", helpOnHelp, pureHelpHandler))]

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
    toLine (cmd, (synopsis, _, _))    = printf "%-20s%s" cmd synopsis
    nopts (CommandLine _ _ opts)   = length opts
    optArg (CommandLine _ _ opts)  =
      case head opts of
        (Option _ (Just _)) -> True
        _                   -> False

-- | A monadic command handler that return the empty printout and the
-- same state as given as input
monadicDoNothingHandler :: MonadicCommandHandler
monadicDoNothingHandler state = return (Right ([], state))

helpOnHelp :: Help
helpOnHelp = [
  "SYNTAX"
  , " help [COMMAND]"
  , ""
  , "DESCRIPTION"
  , " With no COMMAND given a listing of all registered local and remote"
  , " commands are printed on standard output."
  , ""
  , " If a COMMAND is given as option the help printout for the command"
  , " is printed on standard output."
  , ""
  , "EXAMPLES"
  , " help help"
  , " Print this help screen on standard output"
  ]