module Y2017.D03Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2017.D03

tests :: TestTree
tests = testGroup "Tests for day 3, 2017"
        [ testCase "example 1 test" (numToPoint 1 @?= Point 0 0)
        , testCase "example 2 test" (numToPoint 12 @?= Point 2 1)
        , testCase "example 3 test" (numToPoint 23 @?= Point 0 (-2))
        ]
