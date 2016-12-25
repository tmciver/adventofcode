module Y2016.M12.D21 where

{-

--- Day 21: Scrambled Letters and Hash ---

The computer system you're breaking into uses a weird scrambling function to
store its passwords. It shouldn't be much trouble to create your own scrambled
password so you can add it to the system; you just have to implement the
scrambler.

The scrambling function is a series of operations (the exact list is provided in
your puzzle input). Starting with the password to be scrambled, apply each
operation in succession to the string. The individual operations behave as
follows:

* swap position X with position Y means that the letters at indexes X and Y
  (counting from 0) should be swapped.
* swap letter X with letter Y means that the letters X and Y should be swapped
  (regardless of where they appear in the string).
* rotate left/right X steps means that the whole string should be rotated; for
  example, one right rotation would turn abcd into dabc.
* rotate based on position of letter X means that the whole string should be
  rotated to the right based on the index of letter X (counting from 0) as
  determined before this instruction does any rotations. Once the index is
  determined, rotate the string to the right one time, plus a number of times
  equal to that index, plus one additional time if the index was at least 4.
* reverse positions X through Y means that the span of letters at indexes X
  through Y (including the letters at X and Y) should be reversed in order.
* move position X to position Y means that the letter which is at index X should
  be removed from the string, then inserted such that it ends up at index Y.

For example, suppose you start with abcde and perform the following operations:

* swap position 4 with position 0 swaps the first and last letters, producing
  the input for the next step, ebcda.
* swap letter d with letter b swaps the positions of d and b: edcba.
* reverse positions 0 through 4 causes the entire string to be reversed,
  producing abcde.
* rotate left 1 step shifts all letters left one position, causing the first
  letter to wrap to the end of the string: bcdea.
* move position 1 to position 4 removes the letter at position 1 (c), then
  inserts it at position 4 (the end of the string): bdeac.
* move position 3 to position 0 removes the letter at position 3 (a), then
  inserts it at position 0 (the front of the string): abdec.
* rotate based on position of letter b finds the index of letter b (1), then
  rotates the string right once plus a number of times equal to that index (2):
  ecabd.
* rotate based on position of letter d finds the index of letter d (4), then
  rotates the string right once, plus a number of times equal to that index, plus
  an additional time because the index was at least 4, for a total of 6 right
  rotations: decab.

After these steps, the resulting scrambled password is decab.

Now, you just need to generate a new scrambled password and you can access the
system. Given the list of scrambling operations in your puzzle input, what is
the result of scrambling abcdefgh?

-}

import Control.Monad as M
import Prelude hiding (Either(..))
import qualified Data.Either as E
import Data.Char
import Data.List
import Data.Vector as V

type Position = Int
type Steps = Int

data Direction = Left | Right deriving (Eq, Ord, Show)

data Op = SwapPositions Position Position
        | SwapChars Char Char
        | RotateAbsolute Direction Steps
        | RotateRelative Char
        | Reverse Position Position
        | Move Position Position
        deriving (Eq, Ord, Show)

eval :: Vector Char -> Op -> Maybe (Vector Char)
eval v (SwapPositions p1 p2) = do
  c1 <- v !? p1
  c2 <- v !? p2
  return (v // [(p1, c2), (p2, c1)])
eval v (SwapChars c1 c2) = Just $ fmap f v
  where f c
          | c == c1 = c2
          | c == c2 = c1
          | otherwise = c
eval v (RotateAbsolute dir steps) = case dir of
  Left -> Just $ (V.drop normSteps v) V.++ (V.take normSteps v)
  Right -> eval v (RotateAbsolute Left ((V.length v) - normSteps))
  where normSteps = mod steps (V.length v)
eval v (RotateRelative c) = maybeSteps >>= (\steps -> eval v (RotateAbsolute Right steps))
  where maybeSteps = fmap adjustSteps (V.elemIndex c v)
        adjustSteps i = 1 + i + if i >= 4 then 1 else 0
eval v (Reverse p1 p2) = Just $ front V.++ (V.reverse middle) V.++ rear
  where (front, rest) = V.splitAt p1' v
        (middle, rear) = V.splitAt (p2' - p1' + 1) rest
        [p1',p2'] = sort [p1,p2]
eval v (Move p1 p2) = (insert p2) <$> (remove p1 v)
  where remove :: Int -> Vector a -> Maybe (a, Vector a)
        remove i v = (\x -> (x, (V.take i v) V.++ (V.drop (i + 1) v))) <$> (v !? i)
        insert :: Int -> (a, Vector a) -> Vector a
        insert i (x,v) = (V.take i v) V.++ (singleton x) V.++ (V.drop i v)

run :: [Op] -> String -> Maybe String
run ops s = toList <$> M.foldM eval (fromList s) ops

data OpParseError = MalformedCommandString String
                  | NonCommandString String
                  deriving (Eq, Ord, Show)

parse :: String -> E.Either OpParseError Op
parse s | isPrefixOf "swap position" s =
            let v = fromList s
                maybeOp = do
                  i <- digitToInt <$> v !? 14
                  j <- digitToInt <$> v !? 30
                  return $ SwapPositions i j
            in
              maybe (E.Left (MalformedCommandString "Could not parse one or both of the indexes for a 'swap position' command.")) E.Right maybeOp
        | isPrefixOf "swap letter" s =
            let v = fromList s
                maybeOp = do
                  c <- v !? 12
                  d <- v !? 26
                  return $ SwapChars c d
            in
              maybe (E.Left (MalformedCommandString "Could not parse one or both of the letters for a 'swap letters' command.")) E.Right maybeOp
        | isPrefixOf "rotate based on position of letter" s =
            let v = fromList s
                maybeOp = do
                  c <- v !? 35
                  return $ RotateRelative c
            in
              maybe (E.Left (MalformedCommandString "Could not parse letter for a 'rotate relative' command.")) E.Right maybeOp
        | otherwise = E.Left (NonCommandString s)
