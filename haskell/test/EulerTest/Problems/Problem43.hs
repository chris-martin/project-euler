module EulerTest.Problems.Problem43 where

import EulerTest.Prelude
import Euler.Problems.Problem43

tests :: [Test]
tests =
  [ testGroup "problem 43"
    [ testCase "substrings" $
        substrings 1406357289 @?= [406, 63, 635, 357, 572, 728, 289]
    ]
  ]
