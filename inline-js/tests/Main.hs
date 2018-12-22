import Test.Tasty (defaultMain, testGroup)
import qualified Tests.Evaluation as Evaluation
import qualified Tests.PingPong as PingPong

main :: IO ()
main = do
  tests <- sequence [Evaluation.tests, PingPong.tests]
  defaultMain $ testGroup "inline-js Test Suite" tests
