{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.Examples.Stream where

import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import Language.JavaScript.Inline
import Language.JavaScript.Inline.Examples.Utils.LazyIO

lazyStream :: Session -> JSVal -> IO LBS.ByteString
lazyStream _session _stream = do
  LazyIO {..} <- newLazyIO
  _on_data <- export _session (lazyOnData . LBS.toStrict)
  _on_end <- export _session lazyOnEnd
  _on_error <-
    export
      _session
      (lazyOnError . toException . userError . show @EncodedString)
  lazySetFinalizer $ for_ [_on_data, _on_end, _on_error] freeJSVal
  eval @()
    _session
    [js|
      $_stream.on("data", $_on_data);
      $_stream.on("end", $_on_end);
      $_stream.on("error", $_on_error);
    |]
  pure lazyContent
