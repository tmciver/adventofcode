import Test.Tasty
import qualified Y2016.D21Test
import qualified Y2017.WarmUpSpec
import qualified Y2017.D01Test
import qualified Y2017.D02Test
import qualified Y2017.D03Test

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ Y2016.D21Test.tests
                          , Y2017.WarmUpSpec.tests
                          , Y2017.D01Test.tests
                          , Y2017.D02Test.tests
                          , Y2017.D03Test.tests
                          ]


