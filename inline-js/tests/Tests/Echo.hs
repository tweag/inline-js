module Tests.Echo
  ( tests
  ) where

import Control.Monad hiding (fail)
import Control.Monad.Fail
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Language.JavaScript.Inline.Transport.Process
import Language.JavaScript.Inline.Transport.Type
import Language.JavaScript.Inline.Transport.Utils
import qualified Paths_inline_js_core
import Prelude hiding (fail)
import System.FilePath
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: IO TestTree
tests =
  pure $
  withResource
    (do _datadir <- Paths_inline_js_core.getDataDir
        t <-
          newProcessTransport
            ProcessTransportOpts
              { procPath = "node"
              , procArgs =
                  [ "--experimental-modules"
                  , _datadir </> "jsbits" </> "main.mjs"
                  ]
              , procStdErrInherit = True
              }
        lockTransport $ strictTransport t)
    closeTransport
    (\gt ->
       testProperty "Echo via process transport" $
       withMaxSuccess 8 $
       monadicIO $ do
         t <- liftIO gt
         forAllM (vectorOf 65536 $ LBS.pack <$> vectorOf 64 arbitrary) $ \bufs -> do
           bufs' <-
             liftIO $ do
               for_ bufs $ sendData t
               replicateM (length bufs) $ recvData t
           unless (bufs' == bufs) $ fail "echo: mismatch")
