module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Problem4 (answer)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
    testGroup "Problem 4" [
      testCase "answer is correct" (906609 @=? answer)
    ]
  ]