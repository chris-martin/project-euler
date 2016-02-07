module EulerTest.Util.Prime where

import Test.HUnit ((@?=))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Data.Function ( on )
import Data.List     ( sort, sortBy )

import qualified Data.Map as Map

import Euler.Util.Prime

tests :: [Test]
tests = [ primeFactorsTest
        , largestPrimeFactorTest
        , factorizationsTest
        , properDivisorsOfPrimeProductTest
        ]

primeFactorsTest :: Test
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

largestPrimeFactorTest :: Test
largestPrimeFactorTest = testGroup "largestPrimeFactor" $ map t xs where
    xs = [ ( 2,  2)
         , ( 3,  3)
         , ( 4,  2)
         , (47, 47)
         , (94, 47)
         , (99, 11)
         ]
    t (x, y) = testCase (show x) $ largestPrimeFactor x @?= y

factorizationsTest :: Test
factorizationsTest = testGroup "factorizations" $ map t [1..20] where
    t n = testCase ("to " ++ show n) $
            factorizations n @?= (Map.fromList $ map (\i -> (i, primeFactors i)) [1..n])

-- The example from problem 21
properDivisorsOfPrimeProductTest :: Test
properDivisorsOfPrimeProductTest = testGroup "properDivisorsOfPrimeProduct"
    [ testCase "220" $ xs @?= [1, 2, 4, 5, 10, 11, 20, 22, 44, 55, 110] ]
    where xs = sort $ properDivisorsOfPrimeProduct $ primeFactors 220
