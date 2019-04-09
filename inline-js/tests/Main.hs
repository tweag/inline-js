import Test.Tasty (defaultMain, testGroup)
import qualified Tests.Evaluation as Evaluation
import qualified Tests.PingPong as PingPong
import qualified Tests.Quotation as Quotation

main :: IO ()
main = do
  tests <- sequence [Evaluation.tests, PingPong.tests, Quotation.tests]
  defaultMain $ testGroup "inline-js Test Suite" tests
