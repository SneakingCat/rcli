module CommandStateGen (
  stateWithLocal
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
      locals    = fromList <$> ((:) <$> realHandler <*> dummyHandlers)
      remotes   = fromList <$> dummyHandlers
  in CommandState <$> variables <*> locals <*> remotes <*> locals
  where
    realHandler = case lookup cmd localHandlers of
      Just (s, h, f) -> return (cmd, (s, h, f))
      Nothing        -> error $ "Cannot find handler " ++ cmd
    dummyHandlers = filter (\(x, _) -> x /= cmd && x /= excl) <$> listOf handler

-- | Generate a variable
variable :: Gen (String, Value)
variable = (,) <$> identifier <*> value

-- | Generate a handler
handler :: Gen CommandHandlerEntry
handler = (,) <$> identifier 
              <*> ((,,) <$> arbitrary 
                        <*> arbitrary 
                        <*> pure dummyHandler)
              
-- | A dummy command handler. Shall never be executed
dummyHandler :: PureCommandHandler
dummyHandler _ _ = error "Dummy handler. Shall never be called."