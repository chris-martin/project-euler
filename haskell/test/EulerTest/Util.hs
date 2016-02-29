module EulerTest.Util (tests) where

import EulerTest.Prelude

import qualified EulerTest.Util.List

tests :: [Test]
tests = concat
    [ EulerTest.Util.List.tests
    ]
