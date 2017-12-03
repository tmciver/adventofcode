module Y2017.D01Test ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2017.D01

tests :: TestTree
tests = testGroup "Tests for day 1, 2017" [ testCaptcha
                                          , testStringToInts
                                          , testAnswer
                                          ]

testCaptcha :: TestTree
testCaptcha = testGroup "`captcha` function test"
              [ testCase "example 1" $ captcha [1,1,2,2] @?= 3
              , testCase "example 2" $ captcha [1,1,1,1] @?= 4
              , testCase "example 3" $ captcha [1,2,3,4] @?= 0
              , testCase "example 4" $ captcha [9,1,2,1,2,1,2,9] @?= 9
              ]

testStringToInts :: TestTree
testStringToInts = testGroup "`stringToInts` function test" $
                   [ testCase "test success" $ stringToInts "123" @?= [1,2,3]
                   , testCase "test failure" $ stringToInts "123a" @?= []]

testAnswer :: TestTree
testAnswer = testCase "answer test" $ do
  s <- readFile "test/Y2017/D01Input.txt"
  let answer = captcha $ stringToInts (init s)
      expectedAnswer = 1182
  answer @?= expectedAnswer
