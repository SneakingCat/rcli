module Main where

import LineParserTest
import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "LineParser tests"  [
     testProperty "Equality when converting formats" prop_conversionIsReciprocal
     ]
  ]