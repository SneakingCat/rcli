module Main where

import CommandLineTest
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "CommandLine tests"  [
     testProperty "Equality when converting formats" prop_conversionIsReciprocal
     ]
  ]