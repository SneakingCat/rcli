module CommandHandlerTest where

import CommandLineGen ()
import System.Console.RemoteCLI.CommandState (CommandState, empty)
import System.Console.RemoteCLI.CommandHandler (lookupHandler)
import System.Console.RemoteCLI.CommandLine (CommandLine (..))

-- | When the state is empty, the string "Command "X" not found"
-- always shall be returned
prop_commandNotFoundOnEmpty :: CommandLine -> Bool
prop_commandNotFoundOnEmpty cl =
  case lookupHandler cl empty of
    Left [err] -> err == "Command " ++ show (name cl) ++ " not found"
    _          -> False
    where
      name (CommandLine _ n _) = n

