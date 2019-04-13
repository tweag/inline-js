import Test.Tasty (defaultMain, testGroup)
import qualified Tests.Echo as Echo
import qualified Tests.Evaluation as Evaluation
import qualified Tests.LeftPad as LeftPad
import qualified Tests.PingPong as PingPong
import qualified Tests.Quotation as Quotation
import qualified Tests.Wasm as Wasm

main :: IO ()
main = do
  tests <-
    sequence
      [ Echo.tests
      , Evaluation.tests
      , LeftPad.tests
      , PingPong.tests
      , Quotation.tests
      , Wasm.tests
      ]
  defaultMain $ testGroup "inline-js Test Suite" tests
