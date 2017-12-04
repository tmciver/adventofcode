module Y2017.D02Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2017.D02
import Data.List.NonEmpty

tests :: TestTree
tests = testGroup "Tests for day 2, 2017"
        [ parseRowTest ]

parseRowTest :: TestTree
parseRowTest = testCase "`parseRow` function test" $ do
  let input = "123\t456\t789"
      expected = Just $ 123 :| [456, 789]
  parseRow input @?= expected
