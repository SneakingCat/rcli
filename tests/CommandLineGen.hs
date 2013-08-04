module CommandLineGen (
  arbitrary
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
option = Option <$> identifier <*> value
    
-- | Generate a value
value :: Gen (Maybe Value)
value = oneof [valueNothing, valueNull, valueBool, valueInt, valueString]
  where
    valueNothing = return Nothing
    valueNull    = return (Just Null)
    valueBool    = Just <$> (Bool <$> arbitrary)
    valueInt     = Just <$> (Int <$> arbitrary)
    valueString  = Just <$> (String <$> aString)
    aString      = listOf $ oneof [choose ('a', 'z'), choose ('A', 'Z') 
                                  , choose ('0', '9'), elements "-+/%_,."]