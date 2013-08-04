module CommandHandlerTest where

import CommandLineGen ()
import System.Console.RemoteCLI.CommandState (CommandState (..)
                                              , Printout
                                              , MonadicCommandHandler
                                              , empty)
import System.Console.RemoteCLI.CommandHandler (lookupHandler, localHandlers)
import System.Console.RemoteCLI.CommandLine (CommandLine (..), Scope (..))
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))
import qualified Data.Map.Strict as M

data OnlyHelp = OnlyHelp CommandLine CommandState
              deriving Show

instance Arbitrary OnlyHelp where
  arbitrary = OnlyHelp <$> elements [CommandLine Local "help" []
                                    , CommandLine Default "help" []]
                       <*> stateWithLocal "help"

-- | When the state is empty, the string "Command "X" not found"
-- always shall be returned when looking up the handle
prop_commandNotFoundOnEmpty :: CommandLine -> Bool
prop_commandNotFoundOnEmpty commandLine =
  case lookupHandler commandLine empty of
    Left [err] -> err == "Command " ++ show (name commandLine) ++ " not found"
    _          -> False
    where
      name (CommandLine _ n _) = n
      
-- | The help command, when not given any further argument, shall
-- display each registered command on a separate line. The printout
-- header shall be on the first line and say: "The available commands
-- are:"
prop_helpShallDisplayAllCommands :: OnlyHelp -> Bool
prop_helpShallDisplayAllCommands (OnlyHelp commandLine state) =
  case applyPureHandler commandLine state of
    Left _ -> False
    Right _ -> True
    
-- | Help function to create a state where the given command is real,
-- the others are generated dummies
stateWithLocal :: String -> Gen CommandState
stateWithLocal _ = return empty {localCommands = M.fromList localHandlers
                                , defaultScope = M.fromList localHandlers}
    
-- | Apply a pure handler on the given command
applyPureHandler :: CommandLine -> CommandState -> 
                    Either Printout (Printout
                                    , CommandState
                                    , MonadicCommandHandler)
applyPureHandler commandLine state = do
  pureHandler <- lookupHandler commandLine state
  pureHandler commandLine state
    

