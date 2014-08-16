module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Problem12 (answer, triangles)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
    testGroup "Problem 12" [
      testCase "answer is correct" (76576500 @=? answer),
      testCase "triangles" ([1, 3, 6, 10, 15, 21, 28] @=? take 7 triangles)
    ]
  ]
