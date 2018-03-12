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
import Data.Either
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Mustache
import UnliftIO

js :: QuasiQuoter
js =
  QuasiQuoter
    { quoteExp =
        \js_tmp_str ->
          case compileMustacheText "" $ T.pack js_tmp_str of
            Left err -> runIO $ throwIO err
            Right js_tmp@Template {..} ->
              let fail_tmp =
                    runIO $ throwString $ "Illegal template: " ++ show js_tmp
               in case M.toList templateCache of
                    [(_, ns)] -> do
                      l <-
                        for
                          ns
                          (\case
                             TextBlock s ->
                               Right <$> [|T.pack $(liftString $ T.unpack s)|]
                             EscapedVar (Key [v]) ->
                               Right <$>
                               [|T.decodeUtf8 $
                                 LBS.toStrict $
                                 encode $(pure $ VarE $ mkName $ T.unpack v)|]
                             _ -> pure $ Left ())
                      case partitionEithers l of
                        ([], cs) -> [|mconcat $(pure $ ListE cs)|]
                        _ -> fail_tmp
                    _ -> fail_tmp
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }
