module System.Console.RemoteCLI.CommandHandler (  
  localHandlers
  ) where

import System.Console.RemoteCLI.CommandState (Synopsis
                                              , PureCommandHandler
                                              , MonadicCommandHandler
                                              , localCommands
                                              , remoteCommands)
import Text.Printf (printf)
      
-- | Export all local handlers defined in this module
localHandlers :: [(String, (Synopsis, PureCommandHandler))]
localHandlers = [("help", ("a synopsis", pureHelpHandler))]

-- | The command handler for the "help" command
pureHelpHandler :: PureCommandHandler
pureHelpHandler _ state = Right (printout, state, monadicDoNothingHandler)
  where
    printout = header:(map toLine $ localCommands state)
                       ++ (map toLine $ remoteCommands state)
    header = "The available commands are:"
    toLine (cmd, (synopsis, _)) = printf "%-20s%s" cmd synopsis

-- | A monadic command handler that return the empty printout and the
-- same state as given as input
monadicDoNothingHandler :: MonadicCommandHandler
monadicDoNothingHandler state = return (Right ([], state))