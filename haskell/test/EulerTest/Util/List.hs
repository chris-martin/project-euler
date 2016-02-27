module EulerTest.Util.List where

import qualified Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import EulerTest.Prelude
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
  [ t "empty list"     ""         ""
  , t "single element" "a"        "a"
  , t "remove dupes"   "abbbbcca" "abca"
  ]
  where t a b c = testCase a (dedupe b @?= (c :: String))
