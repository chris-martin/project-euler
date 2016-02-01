module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Euler.Problem.Problem13

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests =
  [ testGroup "Problem 13"
    [ testCase "answer is correct" ("5537376230" @=? answer)
    ]
  ]