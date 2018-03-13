{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.JavaScript.Inline.Splices
  ( js
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.JavaScript.Inline.Internals.Parser

js :: QuasiQuoter
js =
  QuasiQuoter
    { quoteExp =
        \js_tmp_str -> do
          chunks <- runIO $ parseChunksIO js_tmp_str
          exprs <-
            for chunks $ \case
              LitChunk s -> [|T.pack $(liftString s)|]
              QuotedChunk s ->
                [|T.decodeUtf8 $ LBS.toStrict $ encode $(pure $ VarE $ mkName s)|]
          [|mconcat $(pure $ ListE exprs)|]
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }
