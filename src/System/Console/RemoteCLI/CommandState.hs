{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module System.Console.RemoteCLI.CommandState (
  CommandState (..)
  , Synopsis
  , CommandHandlerEntry
  , Printout
  , MonadicCommandHandler
  , PureCommandHandler
  , lookupHandler
  , localCommands
  , remoteCommands
  , empty
  , fromList
  ) where

import System.Console.RemoteCLI.CommandLine (CommandLine (..)
                                             , Scope (..)
                                             , Value)
import Text.Printf (printf)
import qualified Data.Map.Strict as M

-- | Alias for a string shortly describing the intention for a command
type Synopsis            = String

-- | A map from strings to values
type VariableMap         = M.Map String Value

-- | A map from strings to command handlers
type CommandHandlerMap   = M.Map String (Synopsis, PureCommandHandler)

-- | A command handler entry as it will look like when dumping the
-- contents of a command handler map
type CommandHandlerEntry = (String, (Synopsis, PureCommandHandler))

-- | The state for the CLI
data CommandState = CommandState VariableMap       -- Variables
                                 CommandHandlerMap -- Local commands
                                 CommandHandlerMap -- Remote command
                                 CommandHandlerMap -- Default scope
                  deriving (Eq, Show)

-- | The "Printout" type for the CLI, i.e. the content that will be
-- displayed by the eval loop
type Printout = [String]             
             
-- | A monadic function to take care of the 'dirty' aspects of command
-- execution. E.g. network communication
type MonadicCommandHandler = CommandState -> 
                             IO (Either Printout (Printout, CommandState))

-- | A pure function to take care of the 'pure' aspects of command
-- execution. E.g. indata checking, state manipulation and preparation
-- of the monadic handler
type PureCommandHandler = CommandLine -> CommandState -> 
                          Either Printout (Printout
                                          , CommandState
                                          , MonadicCommandHandler)
                          
-- | Type class Eq instance
instance Eq PureCommandHandler where
  _ == _ = True
  
-- | Type class Show instance
instance Show PureCommandHandler where
  show _ = "PureCommandHandler"  
           
-- | Lookup the pure handler for the given command line and its
-- selected scope
lookupHandler :: CommandLine -> CommandState -> 
                 Either [String] PureCommandHandler
lookupHandler (CommandLine scope cmd _) (CommandState _ local _ deflt) =
  case M.lookup cmd (select scope) of
    Nothing           -> Left [printf "Command \"%s\" not found" cmd]
    Just (_, handler) -> Right handler
    where
      select Local = local
      select _     = deflt
           
-- | Dump the contents of the local commands. Sorted in ascending
-- order based on key
localCommands :: CommandState -> [CommandHandlerEntry]     
localCommands (CommandState _ l _ _) = M.toAscList l

-- | Dump the contents of the remote commands. Sorted in ascending
-- order based on key
remoteCommands :: CommandState -> [CommandHandlerEntry]
remoteCommands (CommandState _ _ r _) = M.toAscList r
      
-- | Create the empty state
empty :: CommandState
empty = CommandState M.empty M.empty M.empty M.empty

-- | Create a map from list (ugly kind of module export :-( )
fromList :: [(String, a)] -> M.Map String a
fromList = M.fromList
             
