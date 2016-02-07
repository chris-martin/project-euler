module EulerTest.Util.Decimal where

import Test.HUnit ((@?=))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Euler.Util.Decimal

tests :: [Test]
tests = [repetendLengthTest]

repetendLengthTest :: Test
repetendLengthTest = testGroup "repetendLength" $ map t xs where
    xs = [ (1/2,  0)
         , (1/3,  1)
         , (1/4,  0)
         , (1/5,  0)
         , (1/6,  1)
         , (1/7,  6)
         , (1/8,  0)
         , (1/9,  1)
         , (1/10, 0)
         ]
    t (r, i) = testCase ("repetendLength " ++ (show r)) $
                        repetendLength r @?= i
