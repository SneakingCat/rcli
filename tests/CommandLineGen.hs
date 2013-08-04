module CommandLineGen (
  arbitrary
  , identifier
  , value
  ) where

import System.Console.RemoteCLI.CommandLine (CommandLine (..), Scope (..), 
                                             Option (..), Value (..))
import Test.QuickCheck
import Control.Applicative ((<$>), (<*>))

instance Arbitrary CommandLine where
  arbitrary =
    CommandLine <$> scope <*> identifier <*> listOf option

-- | Generate the scope tag
scope :: Gen Scope
scope = elements [Local, Default]

-- | Generate an identifier string
identifier :: Gen String
identifier = (:) <$> beginners <*> listOf followers
  where
    beginners = oneof [choose ('a', 'z'), choose ('A', 'Z')]
    followers = oneof [beginners, elements "0123456789_-"]

-- | Generate an option
option :: Gen Option
option = Option <$> identifier <*> maybeValue
    
-- Generate a value         
value :: Gen Value
value = oneof [return Null, Bool <$> arbitrary
              , Int <$> arbitrary, String <$> asciiString]

-- | Generate a maybe value
maybeValue :: Gen (Maybe Value)
maybeValue = oneof [return Nothing, Just <$> value]
                   
-- | Generate an ascii string with the expected character set             
asciiString :: Gen String
asciiString = listOf $ oneof [choose ('a', 'z'), choose ('A', 'Z') 
                             , choose ('0', '9'), elements "-+/%_,."]