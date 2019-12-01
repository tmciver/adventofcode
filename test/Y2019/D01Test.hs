module Y2019.D01Test ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2019.D01

tests :: TestTree
tests = testGroup "Tests for day 1, 2019" [testCalculateFuel]

testCalculateFuel :: TestTree
testCalculateFuel = testGroup "Tests for 'calculateFuel'"
                    [ testCase "for mass of 12" $ calculateFuel 12 @?= 2
                    , testCase "for mass of 14" $ calculateFuel 14 @?= 2
                    , testCase "for mass of 1969" $ calculateFuel 1969 @?= 654
                    , testCase "for mass of 100756" $ calculateFuel 100756 @?= 33583
                    ]
