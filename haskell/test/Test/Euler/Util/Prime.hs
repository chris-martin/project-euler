module Test.Euler.Util.Prime where

import Test.HUnit
import Test.Framework as TF (testGroup, Test)
import Test.Framework.Providers.HUnit

import Euler.Util.Prime

tests :: [TF.Test]
tests =
  [ testGroup "primeFactors"
    [ testCase  "1" $ primeFactors  1 @=? []
    , testCase  "2" $ primeFactors  2 @=? [2]
    , testCase  "3" $ primeFactors  3 @=? [3]
    , testCase  "4" $ primeFactors  4 @=? [2, 2]
    , testCase "24" $ primeFactors 24 @=? [2, 2, 2, 3]
    , testCase "28" $ primeFactors 28 @=? [2, 2, 7]
    ]
  , testGroup "largestPrimeFactor"
    [ testCase  "2" $ largestPrimeFactor  2 @=?  2
    , testCase  "3" $ largestPrimeFactor  3 @=?  3
    , testCase  "4" $ largestPrimeFactor  4 @=?  2
    , testCase "47" $ largestPrimeFactor 47 @=? 47
    , testCase "94" $ largestPrimeFactor 94 @=? 47
    , testCase "99" $ largestPrimeFactor 99 @=? 11
    ]
  ]
