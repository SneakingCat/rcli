module LineParserTest where

import System.Console.RemoteCLI.LineParser (CommandLine (..), Scope (..), 
                                            Option (..),
                                            fromString, toString)
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))

instance Arbitrary CommandLine where
  arbitrary =
    CommandLine <$> scope <*> identifier <*> listOf option

-- | Generate the scope tag
scope :: Gen Scope
scope = elements [Local, Default]

-- | Generate an option
option :: Gen Option
option = Option <$> identifier

-- | Generate an identifier string
identifier :: Gen String
identifier = (:) <$> beginners <*> listOf followers
  where
    beginners = oneof [choose ('a', 'z'), choose ('A', 'Z')]
    followers = oneof [beginners, elements "0123456789_-"]

-- | The property is given a command line in the internal format. When
-- converted to and then from the string format the result shall be
-- equal to the original
prop_conversionIsReciprocal :: CommandLine -> Bool
prop_conversionIsReciprocal cl = cl == (fromString . toString) cl