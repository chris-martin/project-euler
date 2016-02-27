module EulerTest.Problems.Problem46 where

import Test.HUnit ((@?=))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Euler.Problems.Problem46

tests :: [Test]
tests =
  [ testGroup "problem 46"
    [ testCase "squareDoubles" $
        (take 100 squareDoubles) @?= (map (\n -> 2 * n^2) [1..100])
    , testCase "goldbachNumbers" $
        (take 20 goldbachNumbers) @?=
          [4,5,7,9,10,11,13,15,19,20,21,23,25,27,29,31,33,34,35,37]
    ]
  ]
