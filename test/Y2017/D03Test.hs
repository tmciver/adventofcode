module Y2017.D03Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2017.D03

tests :: TestTree
tests = testGroup "Tests for day 3, 2017"
        [ testCase "example 1 test" (addressToManhattanDistance 1 @?= 0)
        , testCase "example 2 test" (addressToManhattanDistance 12 @?= 3)
        , testCase "example 3 test" (addressToManhattanDistance 23 @?= 2)
        , testCase "example 4 test" (addressToManhattanDistance 1024 @?= 31)
        , testCase "answer test" (addressToManhattanDistance 325489 @?= 552)
        ]
