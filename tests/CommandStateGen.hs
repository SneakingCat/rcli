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
                                    <*> dummyHandlers)
      remotes   = fromList <$> dummyHandlers
  in CommandState <$> variables <*> locals <*> remotes <*> locals
  where
    dummyHandlers = filter (\(x, _) -> x /= cmd && x /= excl) <$> listOf handler

-- | Help function to create a state where the given command is
-- ensured to be in either of the scopes
stateWithDummy :: CommandHandlerEntry -> Gen CommandState
stateWithDummy entry =
  let variables = fromList <$> listOf variable
      scope1    = fromList <$> ((:) <$> pure entry <*> listOf handler)
      scope2    = fromList <$> listOf handler
  in
   oneof[CommandState <$> variables <*> scope1 <*> scope2 <*> scope2
        , CommandState <$> variables <*> scope2 <*> scope1 <*> scope1]

stateWithLocalAndDummy :: String -> CommandHandlerEntry -> Gen CommandState
stateWithLocalAndDummy cmd entry@(name, _) =
  let variables = fromList <$> listOf variable
      locals    = fromList <$> ((:) <$> pure (realHandler cmd) 
                                    <*> dummyHandlers)
      remotes   = fromList <$> ((:) <$> pure entry <*> dummyHandlers)
  in CommandState <$> variables <*> locals <*> remotes <*> locals
  where
    dummyHandlers = filter (\(x, _) -> x /= cmd && x /= name) <$> listOf handler

-- | Generate a variable
variable :: Gen (String, Value)
variable = (,) <$> identifier <*> value

-- | Generate a handler
handler :: Gen CommandHandlerEntry
handler = (,) <$> identifier 
              <*> ((,,) <$> arbitrary 
                        <*> arbitrary 
                        <*> pure dummyHandler)
              
-- | Read the specified handler from the real command handler
realHandler :: String -> CommandHandlerEntry
realHandler cmd = 
  case lookup cmd localHandlers of
    Just e  -> (cmd, e)
    Nothing -> error $ "Cannot find handler " ++ cmd

-- | A dummy command handler. Shall never be executed
dummyHandler :: PureCommandHandler
dummyHandler _ _ = error "Dummy handler. Shall never be called."