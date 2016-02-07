module Main (main) where

import Test.HUnit
import Test.Framework as TF (defaultMain)
import Test.Framework.Providers.HUnit

import Euler.Answers
import EulerTest.Answers

main :: IO ()
main = defaultMain [answerTest 21 answer21]
