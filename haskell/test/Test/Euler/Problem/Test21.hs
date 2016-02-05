module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Euler.Problem.Problem21

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests =
  [ testGroup "Problem 21"
    [ testCase "answer is correct" (31626 @=? answer)
    ]
  ]
