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
  testGroup "CommandLine"  [
     testProperty "Equality when converting formats" pFromAndToShallBeEqual
     ]
  , testGroup "CommandState" [
     testProperty "Command not found on empty state" pCommandNotFoundOnEmpty
     , testProperty "Entry not found on empty state" pEntryNotFoundOnEmpty
     , testProperty "Entry shall be found and equal" pEntryShallBeFoundAndEqual
     ]
  , testGroup "CommandHandler" [
     testProperty "Help shall display all commands" pHelpShallDisplayAllCommands
     , testProperty "Help shall display error" pHelpShallDisplayErrorMessage
     ]
  ]