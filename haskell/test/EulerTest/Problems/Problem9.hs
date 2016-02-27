module EulerTest.Problems.Problem9 where

import EulerTest.Prelude
import Euler.Problems.Problem9

tests :: [Test]
tests =
  [ testGroup "problem 9"
    [ testCase "answer triple" $ answerTriple @?= (200, 375, 425)
    ]
  ]
