module CommandStateTest where

import CommandLineGen ()
import System.Console.RemoteCLI.CommandState (lookupHandler
                                              , empty)
import System.Console.RemoteCLI.CommandLine (CommandLine (..))

-- | When the state is empty, the string "Command "X" not found"
-- always shall be returned when looking up the handle
prop_commandNotFoundOnEmpty :: CommandLine -> Bool
prop_commandNotFoundOnEmpty commandLine =
  case lookupHandler commandLine empty of
    Left [err] -> err == "Command " ++ show (name commandLine) ++ " not found"
    _          -> False
    where
      name (CommandLine _ n _) = n