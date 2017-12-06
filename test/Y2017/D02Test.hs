module Y2017.D02Test (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Y2017.D02
import Data.List.NonEmpty

tests :: TestTree
tests = testGroup "Tests for day 2, 2017"
        [ parseRowTest
        , rowChecksumTest
        , checksumTest
        , answerTest
        ]

parseRowTest :: TestTree
parseRowTest = testCase "`parseRow` function test" $ do
  let input = "123\t456\t789"
      expected = Just $ 123 :| [456, 789]
  parseRow input @?= expected

rowChecksumTest :: TestTree
rowChecksumTest = testGroup "Tests for `RowSummer`s"
                  [ testCase "`rowChecksum1` function test" $ do
                       let row = 5 :| [1,9,5]
                           expected = 8
                       rowChecksum1 row @?= expected
                  , testCase "`rowChecksum2` function test" $ do
                       let row = 5 :| [9,2,8]
                           expected = 4
                       rowChecksum2 row @?= expected
                  ]

checksumTest :: TestTree
checksumTest = testGroup "Tests for checksums"
               [ testCase "`checksum1` function test" $ do
                    let rows = [ 5 :| [1,9,5]
                               , 7 :| [5,3]
                               , 2 :| [4,6,8]
                               ]
                        expected = 18
                    checksum1 rows @?= expected
               , testCase "`checksum2` function test" $ do
                    let rows = [ 5 :| [9,2,8]
                               , 9 :| [4,7,3]
                               , 3 :| [8,6,5]
                               ]
                        expected = 9
                    checksum2 rows @?= expected
               ]

answerTest :: TestTree
answerTest = testGroup "answer tests"
             [ testCase "test for answer to first part" $ do
                  s <- readFile "test/Y2017/D02Input.txt"
                  let spreadsheet = parseRows s
                      sum = checksum1 spreadsheet
                      expectedSum = 42299
                  sum @?= expectedSum
             , testCase "test for answer to second part" $ do
                  s <- readFile "test/Y2017/D02Input.txt"
                  let spreadsheet = parseRows s
                      sum = checksum2 spreadsheet
                      expectedSum = 277
                  sum @?= expectedSum
             ]
