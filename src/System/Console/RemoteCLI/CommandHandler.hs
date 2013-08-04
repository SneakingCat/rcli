module System.Console.RemoteCLI.CommandHandler (
  lookupHandler
  , localHandlers
  ) where

import System.Console.RemoteCLI.CommandLine (CommandLine (..), Scope (..))
import System.Console.RemoteCLI.CommandState (CommandState (..)
                                              , PureCommandHandler
                                              , MonadicCommandHandler)
import Text.Printf
import qualified Data.Map.Strict as M

-- | Lookup the pure handler for the given command line
lookupHandler :: CommandLine -> CommandState -> 
                 Either [String] PureCommandHandler
lookupHandler (CommandLine scope cmd _) state =
  case M.lookup cmd (select scope) of
    Nothing       -> Left [printf "Command \"%s\" not found" cmd]
    Just handler  -> Right handler
    where
      select Local = localCommands state
      select _     = defaultScope state
      
-- | Export all local handlers defined in this module
localHandlers :: [(String, PureCommandHandler)]
localHandlers = [("help", pureHelpHandler)]

pureHelpHandler :: PureCommandHandler
pureHelpHandler commandLine state = Right ([], state, monadicDoNothingHandler)

-- | A monadic command handler that return the empty printout and the
-- same state as given as input
monadicDoNothingHandler :: MonadicCommandHandler
monadicDoNothingHandler state = return (Right ([], state))