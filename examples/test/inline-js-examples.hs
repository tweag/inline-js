{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.IO.Class
import Data.Aeson
import Language.JavaScript.Inline.Configure
import Language.JavaScript.Inline.Import
import Language.JavaScript.Inline.MonadJS

$(js [d|

  foreign import javascript "$(0) * $(1)" mul :: MonadJS m => Double -> Double -> m Double

  foreign import javascript "require('os').tmpdir()" getTmpDir :: MonadJS m => m FilePath

  foreign import javascript "new Promise(resolve => resolve('The answer is: ' + $(0)))" oracle :: (MonadJS m, ToJSON a) => a -> m String

  |])

main :: IO ()
main =
  runJST $(configureOptionsQ) $ do
    answer <- mul 6 7
    liftIO $ print answer
    tmpdir <- getTmpDir
    liftIO $ print tmpdir
    s <- oracle answer
    liftIO $ print s
