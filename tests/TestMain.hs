module Main where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
  testGroup "Dummy tests"  [
     testProperty "Always true" True
     ]
  ]