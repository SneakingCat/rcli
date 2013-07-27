module Main where

import LineParserTest
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "LineParser tests"  [
     testProperty "Equality when convering format" prop_to_then_from_string_eq
     ]
  ]