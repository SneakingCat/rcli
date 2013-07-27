module LineParserTest where

import System.Console.LineParser (CommandLine (..), fromString, toString)
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))

instance Arbitrary CommandLine where
  arbitrary = do
    cmd <- identifier    
    return (CommandLine cmd)

-- | Generate an identifier string
identifier :: Gen String
identifier = (:) <$> letter <*> listOf otherIdChars
  where
    letter = oneof [choose ('a', 'z'), choose ('A', 'Z')]
    otherIdChars = elements "0123456789_-"

-- | The property is given a command line in the internal format. When
-- converted to and then from the string format the result shall be
-- equal to the original
prop_to_then_from_string_eq :: CommandLine -> Bool
prop_to_then_from_string_eq cl = cl == (fromString . toString) cl