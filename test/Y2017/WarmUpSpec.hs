module Y2017.WarmUpSpec ( tests ) where

import Prelude hiding (Either(..))
import Test.Tasty
import Test.Tasty.HUnit
import Y2017.WarmUp
import System.Random (randomIO)
import System.Random.Shuffle (shuffleM)

tests :: TestTree
tests = testGroup "Warm Up Tests" [ testStep
                                  , testStrip
                                  , testMarkers
                                  , testTaxicabDistance
                                  , testStringsToInputs
                                  , testStringToInputs
                                  , answers
                                  ]

testStep :: TestTree
testStep = testGroup "`step` function tests"
           [ testCase "test moving a Point Up" $
             (step (Point 1 2) Up) @?= (Point 1 3)
           , testCase "test moving a Point Right" $
             (step (Point 1 2) Right) @?= (Point 2 2)
           , testCase "test moving a Point Down" $
             (step (Point 1 2) Down) @?= (Point 1 1)
           , testCase "test moving a Point Left" $
             (step (Point 1 2) Left) @?= (Point 0 2)
           ]

testMarkers :: TestTree
testMarkers = testGroup "`markers` function tests"
              [ testMarkers1 ]

testMarkers1 :: TestTree
testMarkers1 = testCase "`markers` function test 1" $ do
  let dirInputs = DirectionInput <$> [ Left, Left, Left, Right, Right, Right, Right -- one more Right than number of Lefts
                                     , Up, Up, Up, Up, Down, Down, Down             -- one more Up than number of Downs
                                     ]
  randomDirs <- shuffleM dirInputs
  randomDirs' <- shuffleM dirInputs
  let input = randomDirs ++ [ButtonInput A] ++ randomDirs' ++ [ButtonInput B]
      expectedMarkers = [Point 1 1, Point 2 2]
  (markers input) @?= expectedMarkers

testTaxicabDistance :: TestTree
testTaxicabDistance = testGroup "`taxicabDistance` function test"
                      [ testTaxicabDistanceFromOrigin
                      , testTaxicabDistanceForPoints]

testTaxicabDistanceFromOrigin = testCase "test taxicab distance from origin" $ do
  x <- randomIO
  y <- randomIO
  (taxicabDistance (Point 0 0) (Point x y) @?= abs x + abs y)

testTaxicabDistanceForPoints = testCase "test taxicab distance for two Points" $ do
  x1 <- randomIO
  y1 <- randomIO
  x2 <- randomIO
  y2 <- randomIO
  let p1 = (Point x1 y1)
      p2 = (Point x2 y2)
  let expectedDistance = abs (x2 - x1) + abs (y2 - y1)
  (taxicabDistance p1 p2) @?= expectedDistance

testStringsToInputs :: TestTree
testStringsToInputs = testGroup "`stringToInput` function test"
                      [ testStringsToInputsSuccess
                      , testStringsToInputsFailure
                      ]

testStringsToInputsSuccess :: TestTree
testStringsToInputsSuccess = testCase "`stringsToInputs` function success test" $
                             let input = ["Left", "Right", "A", "Up", "Down", "B"]
                                 expected = [ DirectionInput Left
                                            , DirectionInput Right
                                            , ButtonInput A
                                            , DirectionInput Up
                                            , DirectionInput Down
                                            , ButtonInput B
                                            ]
                             in
                              stringsToInputs input @?= Just expected

testStringsToInputsFailure :: TestTree
testStringsToInputsFailure = testCase "`stringsToInputs` function failure test" $
                             let input = ["Left", "Rigt", "A", "Up", "Down", "B"]
                             in
                              stringsToInputs input @?= Nothing

testStrip :: TestTree
testStrip = testCase "`strip` function test" $
            strip " Left " @?= "Left"

testStringToInputs :: TestTree
testStringToInputs = testCase "`stringToInputs` function test" $
                     let inStr = "Left, Right, A, Up, Down, B, Start"
                         expected = [ DirectionInput Left
                                    , DirectionInput Right
                                    , ButtonInput A
                                    , DirectionInput Up
                                    , DirectionInput Down
                                    , ButtonInput B
                                    ]
                     in
                      stringToInputs inStr @?= Just expected

answer1 :: IO [Marker] -> TestTree
answer1 markersIo = testCase "verify answer 1" $ do
  markers <- markersIo
  let origin = Point 0 0
      furthestMarkerDistance = furthestFromOrigin markers
      furthestMarkerDistanceExpected = 86
  furthestMarkerDistance @?= furthestMarkerDistanceExpected

answer2 :: IO [Marker] -> TestTree
answer2 markersIo = testCase "verify answer 2" $ do
  markers <- markersIo
  let furthestPairDistance' = furthestPairDistance markers
      furthestPairDistanceExpected = 137
  furthestPairDistance' @?= furthestPairDistanceExpected

answers :: TestTree
answers = withResource getMarkers (const $ return ()) tests
  where getMarkers :: IO [Marker]
        getMarkers = do
          s <- readFile "src/Y2017/WarmUpInput.txt"
          let markers' = maybe [] markers (stringToInputs s)
          return markers'
        tests :: IO [Marker] -> TestTree
        tests ioMarkers = testGroup "test for answers"
                          [ answer1 ioMarkers
                          , answer2 ioMarkers
                          ]
