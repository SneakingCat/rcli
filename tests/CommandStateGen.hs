module CommandStateGen (
  stateWithLocal
  , stateWithDummy
  , stateWithLocalAndDummy
  , variable
  , handler
  ) where

import Test.QuickCheck
import CommandLineGen
import Control.Applicative ((<$>), (<*>), pure)
import System.Console.RemoteCLI.CommandHandler (localHandlers)
import System.Console.RemoteCLI.CommandState (CommandState (..)
                                              , CommandHandlerEntry
                                              , PureCommandHandler
                                              , fromList)
import System.Console.RemoteCLI.CommandLine (Value)
  
-- | Help function to create a state where the given command is a real
-- handler and the others are generated dummies. Also is there an
-- exclusion argument to prevent that a specific dummy command will be
-- present in the state
stateWithLocal :: String -> String -> Gen CommandState
stateWithLocal cmd excl = 
  let variables = fromList <$> listOf variable
      locals    = fromList <$> ((:) <$> pure (realHandler cmd) 
                                    <*> handlers')
      remotes   = fromList <$> handlers'
  in CommandState <$> variables <*> locals <*> remotes <*> locals
  where
    handlers' = exclude cmd excl <$> handlers

-- | Help function to create a state where the given command is
-- ensured to be in either of the scopes
stateWithDummy :: CommandHandlerEntry -> Gen CommandState
stateWithDummy entry =
  let variables = fromList <$> listOf variable
      scope1    = fromList <$> ((:) <$> pure entry <*> handlers)
      scope2    = fromList <$> handlers
  in
   oneof[CommandState <$> variables <*> scope1 <*> scope2 <*> scope2
        , CommandState <$> variables <*> scope2 <*> scope1 <*> scope1]

stateWithLocalAndDummy :: String -> CommandHandlerEntry -> Gen CommandState
stateWithLocalAndDummy cmd entry@(name, _) =
  let variables = fromList <$> listOf variable
      locals    = fromList <$> ((:) <$> pure (realHandler cmd) 
                                    <*> handlers')
      remotes   = fromList <$> ((:) <$> pure entry <*> handlers')
  in CommandState <$> variables <*> locals <*> remotes <*> locals
  where
    handlers' = exclude cmd name <$> handlers

-- | Generate a variable
variable :: Gen (String, Value)
variable = (,) <$> identifier <*> value

-- | Generate a handler
handler :: Gen CommandHandlerEntry
handler = (,) <$> identifier 
              <*> ((,,) <$> arbitrary 
                        <*> arbitrary 
                        <*> pure dummyHandler)
              
-- | Exclude to commands from the list (if present)
exclude :: String -> String -> [CommandHandlerEntry] -> [CommandHandlerEntry]
exclude excl1 excl2 = filter (\(x, _) -> x /= excl1 && x /= excl2)

-- | Generate a list of handlers
handlers :: Gen [CommandHandlerEntry]
handlers = listOf handler

-- | Read the specified handler from the real command handler
realHandler :: String -> CommandHandlerEntry
realHandler cmd = 
  case lookup cmd localHandlers of
    Just e  -> (cmd, e)
    Nothing -> error $ "Cannot find handler " ++ cmd

-- | A dummy command handler. Shall never be executed
dummyHandler :: PureCommandHandler
dummyHandler _ _ = error "Dummy handler. Shall never be called."