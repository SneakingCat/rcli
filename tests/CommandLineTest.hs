module CommandLineTest where

import CommandLineGen ()
import System.Console.RemoteCLI.CommandLine (CommandLine, fromString, toString)

-- | The property is given a command line in the internal format. When
-- converted to and then from the string format the result shall be
-- equal to the original
prop_conversionIsReciprocal :: CommandLine -> Bool
prop_conversionIsReciprocal cl = cl == (fromString . toString) cl