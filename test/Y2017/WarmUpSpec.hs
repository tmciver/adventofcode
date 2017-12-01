module Y2017.WarmUpSpec ( tests ) where

import Prelude hiding (Either(..))
import Test.Tasty
import Test.Tasty.HUnit
import Y2017.WarmUp
import System.Random (randomIO)
import System.Random.Shuffle (shuffleM)

tests :: TestTree
tests = testGroup "Warm Up Tests" [ testStep
                                  , testMarkers
                                  , testTaxicabDistance
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
  (taxicabDistance (Point 0 0) (Point x y) @?= x + y)

testTaxicabDistanceForPoints = testCase "test taxicab distance for two Points" $ do
  x1 <- randomIO
  y1 <- randomIO
  x2 <- randomIO
  y2 <- randomIO
  let expectedDistance = abs (x2 - x1) + abs (y2  - y1)
  (taxicabDistance (Point x1 y1) (Point x2 y2)) @?= expectedDistance
