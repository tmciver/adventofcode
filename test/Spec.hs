
import Prelude hiding (Either(..))
import qualified Data.Either as E
import Test.Tasty
import Test.Tasty.HUnit
import Y2016.M12.D21
import qualified Data.Vector as V

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ evalUnitTests
                          , inversionTests
                          , runUnitTests
                          , parseUnitTests
                          , testPhase1Answer
                          , invertOpTests
                          ]

evalUnitTests :: TestTree
evalUnitTests = testGroup "Tests for `eval` function."
                [ testCase "Test `eval` for `SwapPositions` operator." $
                  (eval (V.fromList "abcde") (SwapPositions 1 3)) `compare` (Just $ V.fromList "adcbe") @?= EQ

                , testCase "Test `eval` for `SwapChars` operator." $
                  (eval (V.fromList "abcde") (SwapChars 'b' 'd')) `compare` (Just $ V.fromList "adcbe") @?= EQ

                , testCase "Test `eval` for `RotateAbsolute` operator with a `Direction` of `Left`." $
                  (eval (V.fromList "abcde") (RotateAbsolute Left 3)) `compare` (Just $ V.fromList "deabc") @?= EQ
                , testCase "Test `eval` for `RotateAbsolute` operator with a `Direction` of `Right`." $
                  (eval (V.fromList "abcde") (RotateAbsolute Right 3)) `compare` (Just $ V.fromList "cdeab") @?= EQ
                , testCase "Test `eval` for `RotateAbsolute` operator with a `Direction` of `Right` and large number of steps." $
                  (eval (V.fromList "abcde") (RotateAbsolute Right 7)) `compare` (Just $ V.fromList "deabc") @?= EQ

                , testCase "Test `eval` for `RotateRelative` operator (1)." $
                  (eval (V.fromList "abcdefghijk") (RotateRelative Right 'b')) `compare` (Just $ V.fromList "jkabcdefghi") @?= EQ
                , testCase "Test `eval` for `RotateRelative Right` operator (2)." $
                  (eval (V.fromList "abcdefghijk") (RotateRelative Right 'e')) `compare` (Just $ V.fromList "fghijkabcde") @?= EQ
                , testCase "Test `eval` for `RotateRelative Left` operator (3)." $
                  (eval (V.fromList "fghabcde") (RotateRelative Left 'e')) `compare` (Just $ V.fromList "bcdefgha") @?= EQ
                , testCase "Test `eval` for `RotateRelative` operator (4)." $
                  (eval (V.fromList "abcdefgh") (RotateRelative Right 'e')) `compare` (Just $ V.fromList "cdefghab") @?= EQ

                , testCase "Test `eval` for `Reverse` operator." $
                  (eval (V.fromList "abcde") (Reverse 1 3)) `compare` (Just $ V.fromList "adcbe") @?= EQ

                , testCase "Test `eval` for `Move` operator." $
                  (eval (V.fromList "abcde") (Move 1 3)) `compare` (Just $ V.fromList "acdbe") @?= EQ
                ]

inversionTests :: TestTree
inversionTests = let testFn s op = ((eval (V.fromList s) op) >>= (flip eval (invertOp op))) `compare` (Just $ V.fromList s) @?= EQ
                 in
                   testGroup "Tests for `eval`ing inversions of `Op`s."
                   [ testCase "Inversion test for `SwapPositions`." $ testFn "abcde" (SwapPositions 1 3)
                   , testCase "Inversion test for `SwapChars`." $ testFn "abcde" (SwapChars 'b' 'd')
                   , testCase "Inversion test for `RotateAbsolute` operator with a `Direction` of `Left`." $ testFn "abcde" (RotateAbsolute Left 3)
                   , testCase "Inversion test for `RotateAbsolute` operator with a `Direction` of `Right`." $ testFn "abcde" (RotateAbsolute Right 3)
                   , testCase "Inversion test for `RotateRelative` operator with a `Direction` of `Left`." $ testFn "abcde" (RotateRelative Left 'b')
                   , testCase "Inversion test for `RotateRelative` operator with a `Direction` of `Right` (1)." $ testFn "abcde" (RotateRelative Right 'b')
                   , testCase "Inversion test for `RotateRelative` operator with a `Direction` of `Right` (2)." $ testFn "abcdefgh" (RotateRelative Right 'e')
                   , testCase "Inversion test for `Reverse` operator`." $ testFn "abcde" (Reverse 1 3)
                   , testCase "Inversion test for `Move` operator`." $ testFn "abcde" (Move 1 3)
                 ]

runUnitTests :: TestTree
runUnitTests = let ops = [ SwapPositions 0 4
                         , SwapChars 'd' 'b'
                         , Reverse 0 4
                         , RotateAbsolute Left 1
                         , Move 1 4
                         , Move 3 0
                         , RotateRelative Right 'b'
                         , RotateRelative Right 'd'
                         ]
                   invertedOps = invertTransformer ops
               in testGroup "Tests for `run` function."
                  [ testCase "Test for test data given in puzzle description." $
                    (run ops "abcde") `compare` (Just "decab") @?= EQ
                  , testCase "Test for inverting test data given in puzzle description." $
                    (run invertedOps "decab") `compare` (Just "abcde") @?= EQ
                  ]

parseUnitTests :: TestTree
parseUnitTests = testGroup "Tests for `parse` function."
                 [ testCase "Test for parsing of a non-command string (malformed command)." $
                   (parse "blah blah blah") `compare` (E.Left (NonCommandString "blah blah blah")) @?= EQ
                 , testCase "Test for parsing of `SwapPositions` command." $
                   (parse "swap position 2 with position 7") `compare` (E.Right (SwapPositions 2 7)) @?= EQ
                 , testCase "Test for parsing of `SwapChars` command." $
                   (parse "swap letter f with letter a") `compare` (E.Right (SwapChars 'f' 'a')) @?= EQ
                 , testCase "Test for parsing of `RotateRelative` (left) command." $
                   (parse "rotate left 4 steps") `compare` (E.Right (RotateAbsolute Left 4)) @?= EQ
                 , testCase "Test for parsing of `RotateRelative` (right) command." $
                   (parse "rotate right 5 steps") `compare` (E.Right (RotateAbsolute Right 5)) @?= EQ
                 , testCase "Test for parsing of `RotateAbsolute` command." $
                   (parse "rotate based on position of letter g") `compare` (E.Right (RotateRelative Right 'g')) @?= EQ
                 , testCase "Test for parsing of `Reverse` command." $
                   (parse "reverse positions 0 through 2") `compare` (E.Right (Reverse 0 2)) @?= EQ
                 , testCase "Test for parsing of `Move` command." $
                   (parse "move position 0 to position 6") `compare` (E.Right (Move 0 6)) @?= EQ
                 ]

invertOpTests :: TestTree
invertOpTests = testGroup "Tests for the `invertOp` function."
                [ testCase "Test inversion of `SwapPositions`." $
                  (invertOp (SwapPositions 2 5)) `compare` (SwapPositions 2 5) @?= EQ
                , testCase "Test inversion of `SwapChars`." $
                  (invertOp (SwapChars 'a' 'd')) `compare` (SwapChars 'a' 'd') @?= EQ
                , testCase "Test inversion of `RotateAbsolute`." $
                  (invertOp (RotateAbsolute Right 5)) `compare` (RotateAbsolute Left 5) @?= EQ
                , testCase "Test inversion of `RotateRelative`." $
                  (invertOp (RotateRelative Right 'c')) `compare` (RotateRelative Left 'c') @?= EQ
                , testCase "Test inversion of `Reverse`." $
                  (invertOp (Reverse 2 5)) `compare` (Reverse 2 5) @?= EQ
                , testCase "Test inversion of `Move`." $
                  (invertOp (Move 2 5)) `compare` (Move 5 2) @?= EQ
                ]

-- Answer to phase 1 of puzzle is "gbhcefad"
testPhase1Answer :: TestTree
testPhase1Answer = testCase "Test for the phase 1 answer using the given input data." $ do
  content <- readFile "test/input.txt"
  let cmdStrings = lines content
  let eitherOps = sequence $ fmap parse cmdStrings
  (flip run "abcdefgh" <$> eitherOps) `compare` (E.Right (Just "gbhcefad")) @?= EQ
