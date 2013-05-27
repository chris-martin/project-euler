module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Problem2

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
    testGroup "Problem 2" [
      testCase "answer is correct" (4613732 @=? Problem2.answer)
    ]
  ]
