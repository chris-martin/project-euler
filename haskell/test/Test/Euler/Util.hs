module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain)
import Test.Framework.Providers.HUnit

import qualified Test.Euler.Util.Prime
import qualified Test.Euler.Util.TrianglePath

main :: IO ()
main = defaultMain tests where
  tests = concat
    [ Test.Euler.Util.Prime.tests
    , Test.Euler.Util.TrianglePath.tests
    ]
