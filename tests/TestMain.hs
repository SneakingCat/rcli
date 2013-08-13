module Main where

import CommandLineTest
import CommandStateTest
import CommandHandlerTest
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "CommandLine tests"  [
     testProperty "Equality when converting formats" prop_fromAndToShallBeEqual
     ]
  , testGroup "CommandState tests" [
     testProperty "Command not found on empty state" prop_commandNotFoundOnEmpty
     ]
  , testGroup "CommandHandler tests" [
     testProperty "Help shall display all commands" prop_helpShallDisplayAllCommands
     , testProperty "Help shall display error" prop_helpShallDisplayErrorMessage
     ]
  ]