module EulerTest.Problems.Problem43 where

import Test.HUnit ((@?=))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Euler.Problems.Problem43

tests :: [Test]
tests =
  [ testGroup "problem 43"
    [ testCase "substrings" $
        substrings 1406357289 @?= [406, 63, 635, 357, 572, 728, 289]
    ]
  ]
