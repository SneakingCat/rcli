module System.Console.RemoteCLI.CommandHandler (
  localHandlers
  ) where

import System.Console.RemoteCLI.CommandState (PureCommandHandler
                                              , MonadicCommandHandler
                                              , localCommands
                                              , remoteCommands)
import Data.List (sort)
      
-- | Export all local handlers defined in this module
localHandlers :: [(String, PureCommandHandler)]
localHandlers = [("help", pureHelpHandler)]

-- | The command handler for the "help" command
pureHelpHandler :: PureCommandHandler
pureHelpHandler _ state = Right (printout, state, monadicDoNothingHandler)
  where
    printout = header:(sort $ localCommands state)
                      ++(sort $ remoteCommands state)
    header = "The available commands are:"

-- | A monadic command handler that return the empty printout and the
-- same state as given as input
monadicDoNothingHandler :: MonadicCommandHandler
monadicDoNothingHandler state = return (Right ([], state))