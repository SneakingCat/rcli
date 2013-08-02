{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module System.Console.RemoteCLI.CommandState (
  CommandState (..)
  , Printout
  , MonadicCommandHandler
  , PureCommandHandler
  , empty
  ) where

import System.Console.RemoteCLI.CommandLine (CommandLine, Value)
import qualified Data.Map.Strict as M

-- | The state for the CLI
data CommandState = CommandState {
  -- | Variables for use with the CLI
  variables        :: M.Map String Value
  -- | Locally defined, intrinsic, commands for the CLI
  , localCommands  :: M.Map String PureCommandHandler
  -- | Remotely defined commands
  , remoteCommands :: M.Map String PureCommandHandler
  -- | Defined to the remoteCommands if defined, else localCommands
  , defaultScope   :: M.Map String PureCommandHandler
  } deriving Show
             
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
                          
instance Show PureCommandHandler where
  show _ = "CommandLine -> CommandState -> "
           ++ "Either Printout (Printout, CommandState, MonadicCommandHandler)"
           
-- | Create the empty state
empty :: CommandState
empty = CommandState {
  variables = M.empty
  , localCommands = M.empty
  , remoteCommands = M.empty
  , defaultScope = M.empty
  }
