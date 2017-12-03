module Y2017.D01Test ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2017.D01

tests :: TestTree
tests = testGroup "Tests for day 1, 2017" [ testCaptcha ]

testCaptcha :: TestTree
testCaptcha = testGroup "`captcha` function test"
               [ testCase "example 1" $ captcha [1,1,2,2] @?= 3 ]
