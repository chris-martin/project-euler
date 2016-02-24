module Main (main) where

import Test.HUnit
import Test.Framework as TF (defaultMain)
import Test.Framework.Providers.HUnit

import qualified EulerTest.Problems
import qualified EulerTest.Util

main :: IO ()
main = defaultMain $ concat
    [ EulerTest.Util.tests
    , EulerTest.Problems.tests
    ]
