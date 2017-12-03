import Test.Tasty
import qualified Y2016.D21Test
import qualified Y2017.WarmUpSpec

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ Y2016.D21Test.tests
                          , Y2017.WarmUpSpec.tests
                          ]


