
import Prelude hiding (Either(..))
import qualified Data.Either as E
import Test.Tasty
import Test.Tasty.HUnit
import Y2016.M12.D21
import qualified Data.Vector as V

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [evalUnitTests, runUnitTests, parseUnitTests]

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
                  (eval (V.fromList "abcdefghijk") (RotateRelative 'b')) `compare` (Just $ V.fromList "jkabcdefghi") @?= EQ
                , testCase "Test `eval` for `RotateRelative` operator (2)." $
                  (eval (V.fromList "abcdefghijk") (RotateRelative 'e')) `compare` (Just $ V.fromList "fghijkabcde") @?= EQ
                , testCase "Test `eval` for `RotateRelative` operator (3)." $
                  (eval (V.fromList "abcde") (RotateRelative 'e')) `compare` (Just $ V.fromList "eabcd") @?= EQ

                , testCase "Test `eval` for `Reverse` operator." $
                  (eval (V.fromList "abcde") (Reverse 1 3)) `compare` (Just $ V.fromList "adcbe") @?= EQ

                , testCase "Test `eval` for `Move` operator." $
                  (eval (V.fromList "abcde") (Move 1 3)) `compare` (Just $ V.fromList "acdbe") @?= EQ
                ]

runUnitTests :: TestTree
runUnitTests = testGroup "Tests for `run` function."
               [ testCase "Test for test data given in puzzle description." $
                 let ops = [ SwapPositions 0 4
                           , SwapChars 'd' 'b'
                           , Reverse 0 4
                           , RotateAbsolute Left 1
                           , Move 1 4
                           , Move 3 0
                           , RotateRelative 'b'
                           , RotateRelative 'd'
                           ]
                 in
                   (run ops "abcde") `compare` (Just "decab") @?= EQ
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
                   (parse "rotate based on position of letter g") `compare` (E.Right (RotateRelative 'g')) @?= EQ
                 , testCase "Test for parsing of `Reverse` command." $
                   (parse "reverse positions 0 through 2") `compare` (E.Right (Reverse 0 2)) @?= EQ
                 , testCase "Test for parsing of `Move` command." $
                   (parse "move position 0 to position 6") `compare` (E.Right (Move 0 6)) @?= EQ
                 ]
