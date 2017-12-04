module Y2017.D02Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2017.D02
import Data.List.NonEmpty

tests :: TestTree
tests = testGroup "Tests for day 2, 2017"
        [ parseRowTest
        , rowChecksumTest
        ]

parseRowTest :: TestTree
parseRowTest = testCase "`parseRow` function test" $ do
  let input = "123\t456\t789"
      expected = Just $ 123 :| [456, 789]
  parseRow input @?= expected

rowChecksumTest :: TestTree
rowChecksumTest = testCase "`rowChecsum` function test" $ do
  let row = 5 :| [1,9,5]
      expected = 8
  rowChecksum row @?= expected
