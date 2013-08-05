module CommandHandlerTest where

import CommandLineGen
import System.Console.RemoteCLI.CommandState (CommandState (..)
                                              , Printout
                                              , PureCommandHandler
                                              , MonadicCommandHandler
                                              , lookupHandler
                                              , fromList)
import System.Console.RemoteCLI.CommandHandler (localHandlers)
import System.Console.RemoteCLI.CommandLine (CommandLine (..)
                                             , Scope (..)
                                             , Value)
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>), pure)

-- | Data type describing the help command, without any arguments to
-- help
data OnlyHelp = OnlyHelp CommandLine CommandState
              deriving Show

-- | Arbitrary generator for the OnlyHelp data type
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
stateWithLocal cmd = 
  let variables = fromList <$> listOf variable
      locals    = fromList <$> ((:) <$> realHandler <*> dummyHandlers)
      remotes   = fromList <$> dummyHandlers
  in CommandState <$> variables <*> locals <*> remotes <*> locals
  where
    realHandler = case lookup cmd localHandlers of
      Just h  -> return (cmd, h)
      Nothing -> error $ "Cannot find handler " ++ cmd
    dummyHandlers = filter (\(x, _) -> x /= cmd) <$> listOf handler

-- | Generate a variable
variable :: Gen (String, Value)
variable = (,) <$> identifier <*> value

-- | Generate a handler
handler :: Gen (String, PureCommandHandler)
handler = (,) <$> identifier <*> pure dummyHandler

-- | Apply a pure handler on the given command
applyPureHandler :: CommandLine -> CommandState -> 
                    Either Printout (Printout
                                    , CommandState
                                    , MonadicCommandHandler)
applyPureHandler commandLine state = do
  pureHandler <- lookupHandler commandLine state
  pureHandler commandLine state
  
-- | A dummy command handler. Shall never be executed
dummyHandler :: PureCommandHandler
dummyHandler _ _ = error "Dummy handler. Shall never be called."
    

