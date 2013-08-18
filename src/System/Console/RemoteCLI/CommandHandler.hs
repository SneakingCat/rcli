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
import Data.Maybe (isJust)
      
-- | Export all local handlers defined in this module
localHandlers :: [(String, (Synopsis, Help, PureCommandHandler))]
localHandlers = [("help", ("a synopsis", helpOnHelp, pureHelpHandler))]

-- | The command handler for the "help" command
pureHelpHandler :: PureCommandHandler
pureHelpHandler (CommandLine _ _ opts) state
  | length opts > 1       = Left tooManyOpts
  | length opts == 1 
    && hasArg (head opts) = Left hasArgument
  | length opts == 0      = Right (listAll, state, monadicDoNothingHandler)
  | otherwise             = Left ["!!"]
  where
    tooManyOpts = "Error: Too many options":["Usage: help <COMMAND>"]
    hasArgument = ["Error: Help option cannot have argument"]
    listAll     = "The available commands are:":
                  (map lineInListing $ localCommands state) 
                    ++ (map lineInListing $ remoteCommands state)
    lineInListing (cmd, (synopsis, _, _)) = printf "%-20s%s" cmd synopsis
    hasArg (Option _ arg) = isJust arg

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