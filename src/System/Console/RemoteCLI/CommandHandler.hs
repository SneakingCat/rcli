module System.Console.RemoteCLI.CommandHandler (
  localHandlers
  ) where

import System.Console.RemoteCLI.CommandState (PureCommandHandler
                                              , MonadicCommandHandler)
      
-- | Export all local handlers defined in this module
localHandlers :: [(String, PureCommandHandler)]
localHandlers = [("help", pureHelpHandler)]

pureHelpHandler :: PureCommandHandler
pureHelpHandler _ state = Right (["The available commands are:"]
                                  , state, monadicDoNothingHandler)

-- | A monadic command handler that return the empty printout and the
-- same state as given as input
monadicDoNothingHandler :: MonadicCommandHandler
monadicDoNothingHandler state = return (Right ([], state))