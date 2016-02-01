module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain)
import Test.Framework.Providers.HUnit

import qualified Test.Euler.Util.Date
import qualified Test.Euler.Util.List
import qualified Test.Euler.Util.Prime
import qualified Test.Euler.Util.TrianglePath

main :: IO ()
main = defaultMain tests where
  tests = concat
    [ Test.Euler.Util.Date.tests
    , Test.Euler.Util.List.tests
    , Test.Euler.Util.Prime.tests
    , Test.Euler.Util.TrianglePath.tests
    ]
