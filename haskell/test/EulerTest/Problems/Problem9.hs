module EulerTest.Problems.Problem9 where

import Test.HUnit ((@?=))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Euler.Problems.Problem9

tests :: [Test]
tests =
  [ testGroup "problem 9"
    [ testCase "answer triple" $ answerTriple @?= (200, 375, 425)
    ]
  ]
