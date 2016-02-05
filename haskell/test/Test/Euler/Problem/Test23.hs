module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Euler.Problem.Problem23

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests =
  [ testGroup "Problem 23"
    [ testCase "answer is correct" (4179871 @=? answer)
    ]
  ]
