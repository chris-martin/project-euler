module Main (main) where

import EulerTest.Prelude

import qualified EulerTest.Problems
import qualified EulerTest.Util

main :: IO ()
main = defaultMain $ concat
    [ EulerTest.Util.tests
    , EulerTest.Problems.tests
    ]
