module Test.Euler.Util.Prime.FactorEnum where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit

import Data.List (sort)

import Euler.Util.Prime.FactorEnum
import Euler.Util.Prime.Factoring (primeFactors)

tests :: [TF.Test]
tests = [ factorizationsTest
        , carryTest
        , carryUntilLTETest
        , doubleTest
        ]

factorizationsTest :: TF.Test
factorizationsTest = testGroup "factorizations" $ map t [1000, 3000, 5000, 9000] where
    t n = testCase ("to " ++ show n) $ fs @?= map primeFactors [1..n]
          where fs = map snd $ sort $ factorizations n

carryTest :: TF.Test
carryTest = testGroup "carry" $ map t xs where
    xs = [ (2, 3)
         , (3, 5)
         , (4, 3)
         , (5, 7)
         , (6, 9)
         , (7, 11)
         ]
    t (x, y) = testCase (show x) $ carry (intToFint x) @?= intToFint y

carryUntilLTETest :: TF.Test
carryUntilLTETest = testGroup "carryUntilLTE"
    [ testCase "16, 10" $ carryUntilLTE 10 (intToFint 16) @?= Just (intToFint 3)
    , testCase "14, 10" $ carryUntilLTE 10 (intToFint 14) @?= Nothing
    ]

doubleTest :: TF.Test
doubleTest = testGroup "double" $ map t [1..20] where
    t n = testCase (show n) $ double (intToFint n) @?= intToFint (n * 2)
