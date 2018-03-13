{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.Import
  ( js
  ) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable
import Language.Haskell.TH.Syntax
import Language.JavaScript.Inline.Internals.Parser
import Language.JavaScript.Inline.MonadJS

funcSpine :: Type -> [Type]
funcSpine (ForallT _ _ t) = funcSpine t
funcSpine (AppT (AppT ArrowT x) y) = x : funcSpine y
funcSpine (SigT t _) = funcSpine t
funcSpine t = [t]

genJSImport :: String -> Name -> Type -> Q [Dec]
genJSImport impent var ftype = do
  cs <- runIO $ map (fmap read) <$> parseChunksIO impent
  args <- replicateM arity $ newName "arg"
  body <-
    [|eval $
      mconcat
        $(fmap ListE $
          for cs $ \case
            LitChunk c -> [|T.pack $(liftString c)|]
            QuotedChunk i ->
              [|T.decodeUtf8 $ LBS.toStrict $ encode $(pure $ VarE $ args !! i)|])|]
  pure [SigD var ftype, FunD var [Clause (map VarP args) (NormalB body) []]]
  where
    sp = funcSpine ftype
    arity = length sp - 1

js :: Q [Dec] -> Q [Dec]
js dsq = do
  ds <- dsq
  fmap concat $
    for ds $ \case
      ForeignD (ImportF JavaScript _ impent var ftype) ->
        genJSImport impent var ftype
      d -> fail $ "Illegal declaration: " ++ show d
