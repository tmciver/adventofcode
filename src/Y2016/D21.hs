module Y2016.D21 where

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
import Data.List.Safe as Safe
import qualified Data.Vector as V

type Position = Int
type Steps = Int

data Direction = Left | Right deriving (Eq, Ord, Show)

-- Some of these operations are their own inverses, a.k.a. *involutions*, (see
-- https:V.//en.wikipedia.org/wiki/Involution_(mathematics)), some are not.
data Op = SwapPositions Position Position -- involution
        | SwapChars Char Char             -- involution
        | RotateAbsolute Direction Steps
        | RotateRelative Direction Char
        | Reverse Position Position       -- involution
        | Move Position Position
        deriving (Eq, Ord, Show)

type Transformer = [Op]

eval :: V.Vector Char -> Op -> Maybe (V.Vector Char)
eval v (SwapPositions p1 p2) = do
  c1 <- v V.!? p1
  c2 <- v V.!? p2
  return (v V.// [(p1, c2), (p2, c1)])
eval v (SwapChars c1 c2) = Just $ fmap f v
  where f c
          | c == c1 = c2
          | c == c2 = c1
          | otherwise = c
eval v (RotateAbsolute dir steps) = case dir of
  Left -> Just $ (V.drop normSteps v) V.++ (V.take normSteps v)
  Right -> eval v (RotateAbsolute Left ((V.length v) - normSteps))
  where normSteps = mod steps (V.length v)
eval v (RotateRelative dir c) = (V.elemIndex c v) >>= getSteps >>= eval v . (RotateAbsolute dir)
  where getSteps :: Int -> Maybe Steps
        getSteps i = case dir of
          Left -> f i
          Right -> Just $ 1 + i + if i >= 4 then 1 else 0
        -- this function seems to work for strings of length 5 and 8. It is not
        -- known what other lengths will work.
        f :: Int -> Maybe Steps
        f 0 = Just 1
        f 1 = Just 1
        f 2 = Just 6
        f 3 = Just 2
        f 4 = Just 7
        f 5 = Just 3
        f 6 = Just 0
        f 7 = Just 4
        f _ = Nothing
eval v (Reverse p1 p2) = Just $ front V.++ (V.reverse middle) V.++ rear
  where (front, rest) = V.splitAt p1' v
        (middle, rear) = V.splitAt (p2' - p1' + 1) rest
        [p1',p2'] = sort [p1,p2]
eval v (Move p1 p2) = (insert p2) <$> (remove p1 v)
  where remove :: Int -> V.Vector a -> Maybe (a, V.Vector a)
        remove i v = (\x -> (x, (V.take i v) V.++ (V.drop (i + 1) v))) <$> (v V.!? i)
        insert :: Int -> (a, V.Vector a) -> V.Vector a
        insert i (x,v) = (V.take i v) V.++ (V.singleton x) V.++ (V.drop i v)

invertOp :: Op -> Op
invertOp op@(SwapPositions _ _) = op
invertOp op@(SwapChars _ _) = op
invertOp (RotateAbsolute dir steps) = RotateAbsolute (flipDirection dir) steps
invertOp (RotateRelative dir c) = RotateRelative (flipDirection dir) c
invertOp op@(Reverse _ _) = op
invertOp (Move p1 p2) = Move p2 p1

invertTransformer :: Transformer -> Transformer
invertTransformer = reverse . map invertOp

run :: Transformer -> String -> Maybe String
run ops s = V.toList <$> M.foldM eval (V.fromList s) ops

data OpParseError = MalformedCommandString String
                  | NonCommandString String
                  deriving (Eq, Ord, Show)

parse :: String -> E.Either OpParseError Op
parse s | isPrefixOf "swap position" s =
            let maybeOp = SwapPositions <$> (digitToInt <$> s Safe.!! 14) <*> (digitToInt <$> s Safe.!! 30) in
            maybe (E.Left (MalformedCommandString "Could not parse one or both of the indexes for a 'swap position' command.")) E.Right maybeOp
        | isPrefixOf "swap letter" s =
            let maybeOp = SwapChars <$> s Safe.!! 12 <*> s Safe.!! 26 in
              maybe (E.Left (MalformedCommandString "Could not parse one or both of the letters for a 'swap letters' command.")) E.Right maybeOp
        | isPrefixOf "rotate left" s =
            let maybeOp = RotateAbsolute Left . digitToInt <$> s Safe.!! 12 in
              maybe (E.Left (MalformedCommandString "Could not parse index for a 'rotate left' command.")) E.Right maybeOp
        | isPrefixOf "rotate right" s =
            let maybeOp = RotateAbsolute Right . digitToInt <$> s Safe.!! 13 in
              maybe (E.Left (MalformedCommandString "Could not parse index for a 'rotate right' command.")) E.Right maybeOp
        | isPrefixOf "rotate based on position of letter" s =
            let maybeOp = RotateRelative Right <$> s Safe.!! 35 in
              maybe (E.Left (MalformedCommandString "Could not parse letter for a 'rotate relative' command.")) E.Right maybeOp
        | isPrefixOf "reverse positions" s =
            let maybeOp = Reverse <$> (digitToInt <$> s Safe.!! 18) <*> (digitToInt <$> s Safe.!! 28) in
            maybe (E.Left (MalformedCommandString "Could not parse one or both of the indexes for a 'reverse' command.")) E.Right maybeOp
        | isPrefixOf "move position" s =
            let maybeOp = Move <$> (digitToInt <$> s Safe.!! 14) <*> (digitToInt <$> s Safe.!! 28) in
            maybe (E.Left (MalformedCommandString "Could not parse one or both of the indexes for a 'move' command.")) E.Right maybeOp
        | otherwise = E.Left (NonCommandString s)

flipDirection :: Direction -> Direction
flipDirection Left = Right
flipDirection Right = Left
