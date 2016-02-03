module Test.Euler.Util.Prime where

import Test.HUnit
import Test.Framework as TF (testGroup, Test)
import Test.Framework.Providers.HUnit

import Test.Euler.Util.Prime.FactorEnum
import Test.Euler.Util.Prime.Factoring

tests :: [TF.Test]
tests = concat
    [ Test.Euler.Util.Prime.FactorEnum.tests
    , Test.Euler.Util.Prime.Factoring.tests
    ]
