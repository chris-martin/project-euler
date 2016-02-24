module EulerTest.Util.Pentagonal where

import Test.HUnit ((@?=))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Euler.Util.Pentagonal

tests :: [Test]
tests =
  [ testGroup "pentagonals" $ concat
    [ [ testCase "first handful" $
          take 10 pentagonals @?= firstTenPentagonals
      ]
    , map isPentagonalTestN [1..100]
    ]
  ]
  where
    firstTenPentagonals = [1, 5, 12, 22, 35, 51, 70, 92, 117, 145]

    isPentagonalTestN n = testCase
      ("isPentagonal " ++ show n)
      (isPentagonal n @?= elem n firstTenPentagonals)
