module System.Console.RemoteCLI.CommandHandler where

import System.Console.RemoteCLI.CommandLine
import System.Console.RemoteCLI.CommandState
import qualified Data.Map.Strict as M

-- | Lookup the pure handler for the given command line
lookupHandler :: CommandLine -> CommandState -> Either String PureCommandHandler
lookupHandler cl@(CommandLine scope cmd opt) state =
  case M.lookup cmd (select scope) of
    Nothing       -> Left $ "Command " ++ (show cmd) ++ " not found"
    Just handler  -> Right handler
    where
      select Local = localCommands state
      select _     = defaultScope state