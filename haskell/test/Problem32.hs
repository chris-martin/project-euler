module Main (main) where

import Test.HUnit
import Test.Framework as TF (defaultMain)
import Test.Framework.Providers.HUnit

import Euler.Problems
import EulerTest.Problems

main :: IO ()
main = defaultMain [answerTest 32]
