module EulerTest.Util.Arithmetic where

import EulerTest.Prelude
import Euler.Util.Arithmetic

tests :: [Test]
tests =
  [ factorialsTest
  , intSqrtTest
  ]

factorialsTest :: Test
factorialsTest = testGroup "factorials"
    [ testCase "first handful" $ (take 10 factorials) @?=
        [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
    ]

intSqrtTest :: Test
intSqrtTest = testGroup "intSqrt" $ do
    (x, root) <- [ (1, Just 1)
                 , (2, Nothing)
                 , (3, Nothing)
                 , (4, Just 2)
                 , (5, Nothing)
                 , let r = 6756381985 in (r^2 - 1, Nothing)
                 , let r = 6756381985 in (r^2, Just r)
                 , let r = 6756381985 in (r^2 + 1, Nothing)
                 ]
    return (testCase ("intSqrt " ++ (show x) ++ " = " ++ (show root))
                     (intSqrt x @?= root))
