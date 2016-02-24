module EulerTest.Util.Date where

import Test.HUnit ((@=?))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Euler.Util.Date

tests :: [Test]
tests =
  [ testGroup "yearLength"
    [ testCase "1900 isn't a leap year, 365 days as normal" $
               365 @=? yearLength 1900
    , testCase "2000 is a leap year, so there's an extra day" $
               366 @=? yearLength 2000
    ]
  ]