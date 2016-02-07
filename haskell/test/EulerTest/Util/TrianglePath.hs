{-# LANGUAGE OverloadedStrings #-}

module EulerTest.Util.TrianglePath where

import Test.HUnit ((@=?))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.List.NonEmpty as NE

import Euler.Util.TrianglePath

tests :: [Test]
tests =
  [ testGroup "parseTriangle"
    [ testCase "a small triangle" $
               parseTriangle" 1\n2 3"
               @=? NE.fromList [ NE.fromList [2, 3]
                               , NE.fromList [1]
                               ]
    ]
  , testGroup "reduceTriangle"
    [ testCase "a small triangle" $
               reduceTriangle (parseTriangle " 1\n2 3") @=? 4
    , testCase "a larger triangle" $
               reduceTriangle (parseTriangle " 1\n2 3\n7 2 4") @=? 10
    ]
  ]
