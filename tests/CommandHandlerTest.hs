module CommandHandlerTest where

import CommandLineGen
import CommandStateGen
import System.Console.RemoteCLI.CommandState (CommandState (..)
                                              , CommandHandlerEntry
                                              , Printout
                                              , MonadicCommandHandler
                                              , lookupHandler
                                              , localCommands
                                              , remoteCommands)
import System.Console.RemoteCLI.CommandLine (CommandLine (..)
                                             , Option (..))
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>), pure)
import Data.List (sortBy)
import Data.Maybe (isJust)
import Text.Printf (printf)

-- | Data type describing the help command, without any arguments to
-- help
data OnlyHelp = OnlyHelp CommandLine CommandState
              deriving Show
                       
-- | Data type describing the erroneous help command
data ErroneousHelp = ErroneousHelp CommandLine CommandState
                   deriving Show

-- | Arbitrary generator for the OnlyHelp data type
instance Arbitrary OnlyHelp where
  arbitrary = OnlyHelp <$> commandLine <*> stateWithLocal "help" ""
    where
      commandLine = CommandLine <$> scope <*> pure "help" <*> pure []
      
-- | Arbitrary generator for ErroneousHelp data type      
instance Arbitrary ErroneousHelp where
  arbitrary = oneof [tooManyOpts, missingOpt]
    where
      tooManyOpts = 
        ErroneousHelp <$>
            (CommandLine <$> scope
                         <*> pure "help"
                         <*> ((:) <$> option <*> listOf1 option))
                      <*> stateWithLocal "help" ""
      missingOpt = do
        opt <- option
        ErroneousHelp <$>
            (CommandLine <$> scope 
                         <*> pure "help" 
                         <*> pure [opt])
                      <*> stateWithLocal "help" (optName opt)
      optName (Option name _) = name

-- | The help command, not given any further argument
pHelpShallDisplayAllCommands :: OnlyHelp -> Bool
pHelpShallDisplayAllCommands (OnlyHelp commandLine state) =
  case applyPureHandler commandLine state of
    Right (x:xs, state', _) -> 
      -- The first row shall read
      x == "The available commands are:"
      
      -- The rest of the rows shall contain all registered commands
      -- with their synopsis. First the locals and then the
      -- remotes. The command list shall be sorted ascending on the
      -- command name
      && xs == (sorted $ localCommands state)
                ++(sorted $ remoteCommands state)

      -- State shall not have been modified
      && state' == state
      
      -- Fail when applying the handler
    Right ([], _, _) -> False
    Left _           -> False
    where
      sorted :: [CommandHandlerEntry] -> Printout
      sorted             = map toLine . sortBy key
      key :: CommandHandlerEntry -> CommandHandlerEntry -> Ordering
      key c1 c2          = fst c1 `compare` fst c2
      toLine :: CommandHandlerEntry -> String
      toLine (k, (s, _, _)) = printf "%-20s%s" k s
    
-- | The help command, with too many options or with a non existing
-- command as option
pHelpShallDisplayErrorMessage :: ErroneousHelp -> Bool    
pHelpShallDisplayErrorMessage (ErroneousHelp commandLine state) =
  case applyPureHandler commandLine state of
    Left (x:y) -> 
      case commandLine of
        (CommandLine _ _ opts)
          | length opts > 1    ->
              x == "Error: Too many options" 
              && y == ["Usage: help [COMMAND]"]
          | hasArg (head opts) ->
              x == "Error: Help option cannot have argument" 
              && y == ["Usage: help [COMMAND]"]
          | otherwise          ->
              x == "Error: Command \"" ++ optName (head opts) ++ "\" not found"
              && y == []
              
    _          -> False
  where
     hasArg  (Option _ arg)  = isJust arg
     optName (Option name _) = name
    
-- | Apply a pure handler on the given command
applyPureHandler :: CommandLine -> CommandState -> 
                    Either Printout (Printout
                                    , CommandState
                                    , MonadicCommandHandler)
applyPureHandler commandLine state = do
  pureHandler <- lookupHandler commandLine state
  pureHandler commandLine state

