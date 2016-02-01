module Test.Euler.Util.List where

import Test.HUnit
import Test.Framework as TF (testGroup, Test)
import Test.Framework.Providers.HUnit

import qualified Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.List.NonEmpty as NE

import Euler.Util.List

tests :: [TF.Test]
tests =
  [ testGroup "neTails"
    [ testCase  "1" (neTails (NE.fromList [1]) @=? NE.fromList [NE.fromList [1]])
    , testCase  "3" (neTails (NE.fromList [1, 2, 3]) @=? NE.fromList [ NE.fromList [1, 2, 3]
                                                                     , NE.fromList [2, 3]
                                                                     , NE.fromList [3]
                                                                     ])
    ]
  ]
