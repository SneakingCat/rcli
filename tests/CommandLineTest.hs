module CommandLineTest where

import CommandLineGen ()
import System.Console.RemoteCLI.CommandLine (CommandLine, fromString, toString)

-- | The property is given a command line in the internal format. When
-- converted to and then from the string format the result shall be
-- equal to the original
prop_fromAndToShallBeEqual :: CommandLine -> Bool
prop_fromAndToShallBeEqual cl =
  case (fromString . toString) cl of
    Right cl' -> cl == cl'
    _         -> False