{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module System.Console.RemoteCLI.CommandState (
  CommandState (..)
  , Printout
  , MonadicCommandHandler
  , PureCommandHandler
  , lookupHandler
  , empty
  , fromList
  ) where

import System.Console.RemoteCLI.CommandLine (CommandLine (..)
                                             , Scope (..)
                                             , Value)
import Text.Printf (printf)
import qualified Data.Map.Strict as M

-- | The state for the CLI
type VariableMap       = M.Map String Value
type CommandHandlerMap = M.Map String PureCommandHandler

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
                          
instance Eq PureCommandHandler where
  _ == _ = True
instance Show PureCommandHandler where
  show _ = "PureCommandHandler"  
           
-- | Lookup the pure handler for the given command line
lookupHandler :: CommandLine -> CommandState -> 
                 Either [String] PureCommandHandler
lookupHandler (CommandLine scope cmd _) (CommandState _ local _ deflt) =
  case M.lookup cmd (select scope) of
    Nothing       -> Left [printf "Command \"%s\" not found" cmd]
    Just handler  -> Right handler
    where
      select Local = local
      select _     = deflt
           
-- | Create the empty state
empty :: CommandState
empty = CommandState M.empty M.empty M.empty M.empty

-- | Create a map from list (ugly kind of module export :-( )
fromList :: [(String, a)] -> M.Map String a
fromList = M.fromList
             
