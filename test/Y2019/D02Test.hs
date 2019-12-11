module Y2019.D02Test ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2019.D02

tests :: TestTree
tests = testGroup "Tests for day 2, 2019" [ testRunIntInstructions
                                          , testSetElement
                                          , testAdd
                                          , testMultiply
                                          ]

testSetElement :: TestTree
testSetElement = testGroup "Tests for 'setElement'"
                 [ testCase "set element 2 of [1,2,3,4] to 6" $ setElement [1,2,3,4] 2 6 @?= Just [1,2,6,4]
                 , testCase "set element 3 of [1,2,3,4] to 6" $ setElement [1,2,3,4] 3 6 @?= Just [1,2,3,6]
                 , testCase "set element 4 of [1,2,3,4] to 6" $ setElement [1,2,3,4] 4 6 @?= Nothing
                 ]

testAdd :: TestTree
testAdd = testGroup "Tests for 'add'"
          [ testCase "add 1 and 2 and put result at index 5" $ add 0 [3,4,5,1,2,0] @?= Just [3,4,5,1,2,3]
          , testCase "add 2 and 3 and put result at index 5" $ add 0 [3,4,5,2,3,0] @?= Just [3,4,5,2,3,5]
          ]

testMultiply :: TestTree
testMultiply = testGroup "Tests for 'multiply'"
          [ testCase "multiply 1 and 2 and put result at index 5" $ multiply 0 [3,4,5,1,2,0] @?= Just [3,4,5,1,2,2]
          , testCase "multiply 2 and 3 and put result at index 5" $ multiply 0 [3,4,5,2,3,0] @?= Just [3,4,5,2,3,6]
          ]

testRunIntInstructions :: TestTree
testRunIntInstructions = testGroup "Tests for 'runIntInstructions'"
                         [ testCase "for instructions [1,9,10,3,2,3,11,0,99,30,40,50]" $ runIntInstructions [1,9,10,3,2,3,11,0,99,30,40,50] @?= Just [3500,9,10,70,2,3,11,0,99,30,40,50]
                         , testCase "for instructions [1,0,0,0,99]" $ runIntInstructions [1,0,0,0,99] @?= Just [2,0,0,0,99]
                         , testCase "for instructions [2,3,0,3,99]" $ runIntInstructions [2,3,0,3,99] @?= Just [2,3,0,6,99]
                         , testCase "for instructions [2,4,4,5,99,0]" $ runIntInstructions [2,4,4,5,99,0] @?= Just [2,4,4,5,99,9801]
                         , testCase "for instructions [1,1,1,4,99,5,6,0,99]" $ runIntInstructions [1,1,1,4,99,5,6,0,99] @?= Just [30,1,1,4,2,5,6,0,99]
                         ]
