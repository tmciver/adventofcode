
import Prelude hiding (Either(..))
import Test.Tasty
import Test.Tasty.HUnit
import Y2016.M12.D21
import qualified Data.Vector as V

main = defaultMain evalUnitTests

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
