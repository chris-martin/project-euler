module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Problem67 (answer)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests =
  [ testGroup "Problem 67"
    [ testCase "answer is correct" (7273 @=? answer)
    ]
 ]
