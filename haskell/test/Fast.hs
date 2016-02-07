module Main (main) where

import Test.HUnit
import Test.Framework as TF (defaultMain)
import Test.Framework.Providers.HUnit

import qualified EulerTest.Answers
import qualified EulerTest.Util

main :: IO ()
main = defaultMain $ EulerTest.Util.tests ++ [EulerTest.Answers.fastTest]
