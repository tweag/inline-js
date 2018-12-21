import Test.Tasty (defaultMain, testGroup)
import qualified Tests.Quotation as Quotation

main :: IO ()
main = do
  tests <- sequence [Quotation.tests]
  defaultMain $ testGroup "inline-js Test Suite" tests
