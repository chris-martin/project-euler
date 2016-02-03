module Test.Euler.Util.Prime.Factoring where

import Test.HUnit
import Test.Framework as TF (testGroup, Test)
import Test.Framework.Providers.HUnit

import Data.List (sort)

import Euler.Util.Prime.Factoring

tests :: [TF.Test]
tests = [ primeFactorsTest
        , largestPrimeFactorTest
        , properDivisorsOfPrimeProductTest
        ]

primeFactorsTest :: TF.Test
primeFactorsTest = testGroup "primeFactors" $ map t xs where
    xs = [ []
         , [2]
         , [3]
         , [2, 2]
         , [2, 2, 2, 3]
         , [2, 2, 7]
         ]
    t fs = let n = product fs
           in  testCase (show n) $ primeFactors n @?= fs

largestPrimeFactorTest :: TF.Test
largestPrimeFactorTest = testGroup "largestPrimeFactor" $ map t xs where
    xs = [ ( 2,  2)
         , ( 3,  3)
         , ( 4,  2)
         , (47, 47)
         , (94, 47)
         , (99, 11)
         ]
    t (x, y) = testCase (show x) $ largestPrimeFactor x @?= y

-- The example from problem 21
properDivisorsOfPrimeProductTest :: TF.Test
properDivisorsOfPrimeProductTest = testGroup "properDivisorsOfPrimeProduct"
    [ testCase "220" $ xs @?= [1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110]
    ]
    where xs = sort $ properDivisorsOfPrimeProduct $ primeFactors 220
