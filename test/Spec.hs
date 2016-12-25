
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

                , testCase "Test `eval` for `RotateByCharIndex` operator (1)." $
                  (eval (V.fromList "abcdefghijk") (RotateByCharIndex 'b')) `compare` (Just $ V.fromList "jkabcdefghi") @?= EQ
                , testCase "Test `eval` for `RotateByCharIndex` operator (2)." $
                  (eval (V.fromList "abcdefghijk") (RotateByCharIndex 'e')) `compare` (Just $ V.fromList "fghijkabcde") @?= EQ
                , testCase "Test `eval` for `RotateByCharIndex` operator (3)." $
                  (eval (V.fromList "abcde") (RotateByCharIndex 'e')) `compare` (Just $ V.fromList "eabcd") @?= EQ

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
                           , RotateByCharIndex 'b'
                           , RotateByCharIndex 'd'
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
                 ]
