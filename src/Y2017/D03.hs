module Y2017.D03 where

{- |

--- Day 3: Spiral Memory ---

You come across an experimental new kind of memory stored on an infinite
two-dimensional grid.

Each square on the grid is allocated in a spiral pattern starting at a location
marked 1 and then counting up while spiraling outward. For example, the first
few squares are allocated like this:

17  16  15  14  13
18   5   4   3  12
19   6   1   2  11
20   7   8   9  10
21  22  23---> ...

While this is very space-efficient (no squares are skipped), requested data must
be carried back to square 1 (the location of the only access port for this
memory system) by programs that can only move up, down, left, or right. They
always take the shortest path: the Manhattan Distance between the location of
the data and square 1.

For example:

* Data from square 1 is carried 0 steps, since it's at the access port.
* Data from square 12 is carried 3 steps, such as: down, left, left.
* Data from square 23 is carried only 2 steps: up twice.
* Data from square 1024 must be carried 31 steps.

How many steps are required to carry the data from the square identified in your
puzzle input all the way to the access port?

Your puzzle input is 325489.

-}

import Prelude hiding (Either(..))

type Address = Int
data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Eq, Show)
data Direction = Right | Up | Left | Down deriving (Enum)
data SquareElement = SquareElement { side :: Int
                                   , address :: Address
                                   , point :: Point
                                   , direction :: Direction
                                   , distanceAlongSide :: Int
                                   }

nextDirection :: Direction -> Direction
nextDirection Down = Right
nextDirection dir = succ dir

advancePoint :: Point -> Direction -> Point
advancePoint p Up = p { y = y p + 1 }
advancePoint p Right = p { x = x p + 1 }
advancePoint p Down = p { y = y p - 1 }
advancePoint p Left = p { x = x p - 1 }

nextElement :: SquareElement -> SquareElement
nextElement (SquareElement side addr p dir dist) = case dir of
  Right | dist == side -> SquareElement (side + 2) (addr + 1) (advancePoint p Right) Up 2
  dir | dist == side -> SquareElement side (addr + 1) (advancePoint p (nextDirection dir)) (nextDirection dir) 2
  dir -> SquareElement side (addr + 1) (advancePoint p dir) dir (dist + 1)

memory :: [SquareElement]
memory = iterate nextElement initialElement
  where initialElement = SquareElement 1 1 (Point 0 0) Right 1

addressToPoint :: Address -> Point
addressToPoint addr = point $ memory !! (addr - 1)

manhattanDistance :: Point -> Int
manhattanDistance (Point x y) = abs x + abs y

addressToManhattanDistance :: Address -> Int
addressToManhattanDistance = manhattanDistance . addressToPoint
