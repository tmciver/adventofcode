module Y2017.D01Test ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2017.D01

tests :: TestTree
tests = testGroup "Tests for day 1, 2017" [ testCaptcha1
                                          , testStringToInts
                                          , testAnswer1
                                          ]

testCaptcha1 :: TestTree
testCaptcha1 = testGroup "`captcha1` function test"
              [ testCase "example 1" $ captcha1 [1,1,2,2] @?= 3
              , testCase "example 2" $ captcha1 [1,1,1,1] @?= 4
              , testCase "example 3" $ captcha1 [1,2,3,4] @?= 0
              , testCase "example 4" $ captcha1 [9,1,2,1,2,1,2,9] @?= 9
              ]

testStringToInts :: TestTree
testStringToInts = testGroup "`stringToInts` function test" $
                   [ testCase "test success" $ stringToInts "123" @?= [1,2,3]
                   , testCase "test failure" $ stringToInts "123a" @?= []]

testAnswer1 :: TestTree
testAnswer1 = testCase "answer test" $ do
  s <- readFile "test/Y2017/TestInput/D01Input.txt"
  let answer = captcha1 $ stringToInts (init s)
      expectedAnswer = 1182
  answer @?= expectedAnswer
