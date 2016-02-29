module Main (main) where

import EulerTest.Prelude

import qualified EulerTest.Problems

main :: IO ()
main = defaultMain EulerTest.Problems.tests
