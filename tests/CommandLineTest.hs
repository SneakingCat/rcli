module CommandLineTest where

import CommandLineGen ()
import System.Console.RemoteCLI.CommandLine (CommandLine, fromString, toString)

-- | The property is given a command line in the internal format. When
-- converted to and then from the string format the result shall be
-- equal to the original
pFromAndToShallBeEqual :: CommandLine -> Bool
pFromAndToShallBeEqual cl =
  case (fromString . toString) cl of
    Right cl' -> cl == cl'
    _         -> False