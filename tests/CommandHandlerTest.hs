module CommandHandlerTest where

import System.Console.RemoteCLI.CommandState (CommandState (..)
                                              , Printout
                                              , MonadicCommandHandler
                                              , lookupHandler
                                              , empty)
import System.Console.RemoteCLI.CommandHandler (localHandlers)
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
stateWithLocal _ = return empty
    
-- | Apply a pure handler on the given command
applyPureHandler :: CommandLine -> CommandState -> 
                    Either Printout (Printout
                                    , CommandState
                                    , MonadicCommandHandler)
applyPureHandler commandLine state = do
  pureHandler <- lookupHandler commandLine state
  pureHandler commandLine state
    

