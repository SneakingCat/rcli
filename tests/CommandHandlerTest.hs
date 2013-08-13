module CommandHandlerTest where

import CommandLineGen
import System.Console.RemoteCLI.CommandState (CommandState (..)
                                              , CommandHandlerEntry
                                              , Printout
                                              , PureCommandHandler
                                              , MonadicCommandHandler
                                              , lookupHandler
                                              , localCommands
                                              , remoteCommands
                                              , fromList)
import System.Console.RemoteCLI.CommandHandler (localHandlers)
import System.Console.RemoteCLI.CommandLine (CommandLine (..)
                                             , Value)
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>), pure)
import Data.List (sortBy)
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
  arbitrary = OnlyHelp <$> commandLine <*> stateWithLocal "help"
    where
      commandLine = CommandLine <$> scope <*> pure "help" <*> pure []
      
-- | Arbitrary generator for ErroneousHelp data type      
instance Arbitrary ErroneousHelp where
  arbitrary = ErroneousHelp <$> tooManyOpts <*> stateWithLocal "help"
    where
      tooManyOpts = CommandLine <$> scope 
                                <*> pure "help" 
                                <*> ((:) <$> option <*> listOf1 option)
      
-- | The help command, not given any further argument
prop_helpShallDisplayAllCommands :: OnlyHelp -> Bool
prop_helpShallDisplayAllCommands (OnlyHelp commandLine state) =
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
      toLine (k, (s, _)) = printf "%-20s%s" k s
    
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
      Just (s, h) -> return (cmd, (s, h))
      Nothing     -> error $ "Cannot find handler " ++ cmd
    dummyHandlers = filter (\(x, _) -> x /= cmd) <$> listOf handler

-- | Generate a variable
variable :: Gen (String, Value)
variable = (,) <$> identifier <*> value

-- | Generate a handler
handler :: Gen CommandHandlerEntry
handler = (,) <$> identifier <*> ((,) <$> arbitrary <*> pure dummyHandler)

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
