module Y2019.D01Test ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2019.D01

tests :: TestTree
tests = testGroup "Tests for day 1, 2019" [testCalculateFuelMass, testTotalFuelMass]

testCalculateFuelMass :: TestTree
testCalculateFuelMass = testGroup "Tests for 'calculateFuelMass'"
                    [ testCase "for mass of 12" $ calculateFuelMass 12 @?= 2
                    , testCase "for mass of 14" $ calculateFuelMass 14 @?= 2
                    , testCase "for mass of 1969" $ calculateFuelMass 1969 @?= 966
                    , testCase "for mass of 100756" $ calculateFuelMass 100756 @?= 50346
                    ]

testTotalFuelMass :: TestTree
testTotalFuelMass = testCase "total fuel requirement" $
  totalFuelMass "test/Y2019/D01Input.txt" >>= \tf -> tf @?= 4856390
