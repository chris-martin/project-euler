module EulerTest.Util.List where

import Test.HUnit ((@?=))
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Euler.Util.List

tests :: [Test]
tests = [neTailsTest, dedupeTest]

neTailsTest :: Test
neTailsTest = testGroup "neTails"
  [ testCase "1" $
      neTails (NE.fromList [1])
      @?=
      NE.fromList [ NE.fromList [1] ]
  , testCase "3" $
      neTails (NE.fromList [1, 2, 3])
      @?=
      NE.fromList [ NE.fromList [1, 2, 3]
                  , NE.fromList [2, 3]
                  , NE.fromList [3] ]
  ]

dedupeTest :: Test
dedupeTest = testGroup "dedupe"
  [ testCase "empty list"     (dedupe ""         @?= ""    )
  , testCase "single element" (dedupe "a"        @?= "a"   )
  , testCase "remove dupes"   (dedupe "abbbbcca" @?= "abca")
  ]
