module Y2017.WarmUp where

{-

This warm-up problem was announced in this tweet:
https://twitter.com/_sorceress/status/935256907408867328 and described here:
https://pastebin.com/BMd61PUv.

Advent Warmup: Elvish Cheat Codes
---------------------------------

In the run up to advent, the elves are playing on their video game
console. Before long, one of the elves manages to discover a cheat mode, by
entering a sequence of button presses on the controller.

The sequence involves the buttons 'Up', 'Down', 'Left', 'Right', 'A', 'B', and
terminates with a single press of the 'Start' button.

The elves begin to ponder the significance of the sequence they discovered, and
decide to draw a chart.

Starting at the origin in an (x,y) grid, the buttons Up, Down, Left, Right are
imagined to move a cursor a single step in the corresponding direction through
the grid. Buttons A and B place corresponding markers at the current cursor
location.

-------------------------------------------------------------
Example: Up, A, Right, Right, B, Left, B, Start

Taking Right to be the positive x direction, and Up to be the positive y
direction. This sequence will move one step up from the origin (0,0), and place
an 'A' marker at location (0,1), then a 'B' marker at location (2,1). The cursor
will move one step left and another 'B' marker is placed at location (1,1). Then
the cursor halts at location (1,1).

-------------------------------------------------------------
Example: Up, Up, Down, Down, Left, Right, Left, Right, B, A, Start

Again, starting from the origin (0,0), this sequence will place both a 'B' and
an 'A' marker at (0,0), and the cursor will halt at (0,0).

-------------------------------------------------------------

The taxicab distance (https://en.wikipedia.org/wiki/Taxicab_distance) between
two grid locations is defined as the (positive) difference between the two
points' x values + the (positive) difference between their y values.

eg, between locations (1,2) and (8,6), the difference between the two x values
(1 and 8) is 7, and the difference between the two y values (2 and 6) is
4. Therefore, the taxicab distance is 7 + 4 = 11.

Your input is here: https://pastebin.com/wGmzZHeq

Question 1:

Identify the marker furthest from the origin, as measured by the taxicab
distance, and return that distance.

Answer: 86


Question 2:

Consider all pairs of *different* markers (where a pair may consist of any 'A'
and any 'B' marker). Identify the pair maximally far apart from one another, as
measured by the taxicab distance, and return that distance.

Answer: 137

-}

import Prelude hiding (Either(..))
import Data.List.Split (splitOn)
import Data.Char (isSpace)

data Direction = Up | Down | Left | Right deriving (Eq, Show)
data Button =  A | B deriving (Eq, Show)
data Input = DirectionInput Direction
           | ButtonInput Button
           deriving (Eq, Show)
data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Eq, Show)
type Marker = Point

-- | Move the given 'Point' in the given 'Direction' yielding a new 'Point'.
step :: Point -> Direction -> Point
step p Up = p { y = y p + 1 }
step p Right = p { x = x p + 1 }
step p Down = p { y = y p - 1 }
step p Left = p { x = x p - 1 }

-- | Produce the list of 'Marker's from the sequence of 'Input's.
markers :: [Input] -> [Marker]
markers = reverse . snd . foldl f init
          where init = (Point 0 0, [])
                f :: (Point, [Marker]) -> Input -> (Point, [Marker])
                f (p, markers) (DirectionInput dir) = (step p dir, markers)
                f (p, markers) (ButtonInput _) = (p, p:markers)

taxicabDistance :: Point -> Point -> Int
taxicabDistance p1 p2 = abs (x p2 - x p1) + abs (y p2 - y p1)

stringsToInputs :: [String] -> Maybe [Input]
stringsToInputs = mapM f
  where f :: String -> Maybe Input
        f "Left" = Just $ DirectionInput Left
        f "Up" = Just $ DirectionInput Up
        f "Down" = Just $ DirectionInput Down
        f "Right" = Just $ DirectionInput Right
        f "A" = Just $ ButtonInput A
        f "B" = Just $ ButtonInput B
        f _ = Nothing

strip :: String -> String
strip = takeWhile (not . isSpace) . dropWhile isSpace

stringToInputs :: String -> Maybe [Input]
stringToInputs s = stringsToInputs strs
  where strs = init $ map strip (splitOn "," s)

furthestFromOrigin :: [Marker] -> Int
furthestFromOrigin = maximum . map (taxicabDistance origin)
  where origin = Point 0 0

furthestPairDistance :: [Marker] -> Int
furthestPairDistance = maximum . allDistances
  where allDistances :: [Marker] -> [Int]
        allDistances markers = do
          marker1 <- markers
          marker2 <- markers
          return $ taxicabDistance marker1 marker2
