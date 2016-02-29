module EulerTest.Prelude
    (
    -- * Test suite structure
      Test, testGroup, testCase, defaultMain

    -- * Assertions
    , (@?), (@?=), assertFailure
    ) where

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
