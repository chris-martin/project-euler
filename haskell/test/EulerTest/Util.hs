module EulerTest.Util (tests) where

import Test.Framework (Test)

import qualified EulerTest.Util.Date
import qualified EulerTest.Util.Decimal
import qualified EulerTest.Util.List
import qualified EulerTest.Util.Pentagonal
import qualified EulerTest.Util.Prime
import qualified EulerTest.Util.TrianglePath

tests :: [Test]
tests = concat
    [ EulerTest.Util.Date.tests
    , EulerTest.Util.Decimal.tests
    , EulerTest.Util.List.tests
    , EulerTest.Util.Pentagonal.tests
    , EulerTest.Util.Prime.tests
    , EulerTest.Util.TrianglePath.tests
    ]
