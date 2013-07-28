module LineParserTest where

import System.Console.RemoteCLI.LineParser (CommandLine (..), 
                                            fromString, toString)
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))

instance Arbitrary CommandLine where
  arbitrary = do
    cmd <- identifier    
    return (CommandLine cmd)

-- | Generate an identifier string
identifier :: Gen String
identifier = (:) <$> beginners <*> listOf followers
  where
    beginners = oneof [choose ('a', 'z'), choose ('A', 'Z')]
    followers = elements "0123456789_-"

-- | The property is given a command line in the internal format. When
-- converted to and then from the string format the result shall be
-- equal to the original
prop_conversionIsReciprocal :: CommandLine -> Bool
prop_conversionIsReciprocal cl = cl == (fromString . toString) cl