module LineParserTest where

import System.Console.LineParser (CommandLine (..), fromString, toString)
import Test.QuickCheck

instance Arbitrary CommandLine where
  arbitrary = return (CommandLine "Sigge")

-- | The property is given a command line in the internal format. When
-- converted to and then from the string format the result shall be
-- equal to the original
prop_to_then_from_string_eq :: CommandLine -> Bool
prop_to_then_from_string_eq cl = cl == (fromString . toString) cl