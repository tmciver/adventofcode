module Y2019.D02Test ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2019.D02

tests :: TestTree
tests = testGroup "Tests for day 2, 2019" [testRunIntInstructions]

testRunIntInstructions :: TestTree
testRunIntInstructions = testGroup "Tests for 'runIntInstructions'"
                         [ testCase "for instructions [1,9,10,3,2,3,11,0,99,30,40,50]" $ runIntInstructions [1,9,10,3,2,3,11,0,99,30,40,50] @?= [3500,9,10,70,2,3,11,0,99,30,40,50]
                         , testCase "for instructions [1,0,0,0,99]" $ runIntInstructions [1,0,0,0,99] @?= [2,0,0,0,99]
                         , testCase "for instructions [2,3,0,3,99]" $ runIntInstructions [2,3,0,3,99] @?= [2,3,0,6,99]
                         , testCase "for instructions [2,4,4,5,99,0]" $ runIntInstructions [2,4,4,5,99,0] @?= [2,4,4,5,99,9801]
                         , testCase "for instructions [1,1,1,4,99,5,6,0,99]" $ runIntInstructions [1,1,1,4,99,5,6,0,99] @?= [30,1,1,4,2,5,6,0,99]
                         ]
