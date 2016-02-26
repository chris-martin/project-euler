module EulerTest.Util.FigurateNumbers where

import Test.HUnit ((@?=))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Euler.Util.FigurateNumbers

tests :: [Test]
tests = [triangleTest, pentagonalTest, hexagonalTest]

triangleTest :: Test
triangleTest = testGroup "triangles" $ concat
  [ [ testCase "first handful" $
      take 5 triangles @?= firstFiveTriangles
    ]
  , map isTriangleTestN [1..20]
  ]
  where
    firstFiveTriangles = [1, 3, 6, 10, 15]

    isTriangleTestN n = testCase
      ("isTriangle " ++ show n)
      (isTriangle n @?= elem n firstFiveTriangles)

pentagonalTest :: Test
pentagonalTest = testGroup "pentagonals" $ concat
  [ [ testCase "first handful" $
      take 10 pentagonals @?= firstTenPentagonals
    ]
  , map isPentagonalTestN [1..100]
  ]
  where
    firstTenPentagonals = [1, 5, 12, 22, 35, 51, 70, 92, 117, 145]

    isPentagonalTestN n = testCase
      ("isPentagonal " ++ show n)
      (isPentagonal n @?= elem n firstTenPentagonals)

hexagonalTest :: Test
hexagonalTest = testGroup "hexagonals" $ concat
  [ [ testCase "first handful" $
      take 5 hexagonals @?= firstFiveHexagonals
    ]
  , map isHexagonalTestN [1..50]
  ]
  where
    firstFiveHexagonals = [1, 6, 15, 28, 45]

    isHexagonalTestN n = testCase
      ("isHexagonal " ++ show n)
      (isHexagonal n @?= elem n firstFiveHexagonals)
