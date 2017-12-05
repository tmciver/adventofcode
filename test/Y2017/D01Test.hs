module Y2017.D01Test ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2017.D01

tests :: TestTree
tests = testGroup "Tests for day 1, 2017" [ testCaptcha1
                                          , testCaptcha2
                                          , testStringToInts
                                          , testAnswer
                                          ]

testCaptcha1 :: TestTree
testCaptcha1 = testGroup "`captcha1` function test"
              [ testCase "example 1" $ captcha1 [1,1,2,2] @?= 3
              , testCase "example 2" $ captcha1 [1,1,1,1] @?= 4
              , testCase "example 3" $ captcha1 [1,2,3,4] @?= 0
              , testCase "example 4" $ captcha1 [9,1,2,1,2,1,2,9] @?= 9
              ]

testCaptcha2 :: TestTree
testCaptcha2 = testGroup "`captcha2` function test"
              [ testCase "example 1" $ captcha2 [1,2,1,2] @?= 6
              , testCase "example 2" $ captcha2 [1,2,2,1] @?= 0
              , testCase "example 3" $ captcha2 [1,2,3,4,2,5] @?= 4
              , testCase "example 4" $ captcha2 [1,2,3,1,2,3] @?= 12
              ]

testStringToInts :: TestTree
testStringToInts = testGroup "`stringToInts` function test" $
                   [ testCase "test success" $ stringToInts "123" @?= [1,2,3]
                   , testCase "test failure" $ stringToInts "123a" @?= []]

testAnswer :: TestTree
testAnswer = testGroup "test for answers"
             [ testCase "answer 1 test" $ do
                  s <- readFile "test/Y2017/TestInput/D01Input.txt"
                  let answer = captcha1 $ stringToInts (init s)
                      expectedAnswer = 1182
                  answer @?= expectedAnswer
             , testCase "answer 2 test" $ do
                  s <- readFile "test/Y2017/TestInput/D01Input.txt"
                  let answer = captcha2 $ stringToInts (init s)
                      expectedAnswer = 1152
                  answer @?= expectedAnswer
                  --putStrLn $ "answer 2: " ++ show answer
             ]
