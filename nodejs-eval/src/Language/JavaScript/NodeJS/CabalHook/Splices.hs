module Language.JavaScript.NodeJS.CabalHook.Splices
  ( datadirQ
  ) where

import Data.Binary
import Language.Haskell.TH

datadirQ :: Q Exp
datadirQ = do
  datadir <- runIO $ decodeFile ".buildinfo"
  pure $ LitE $ StringL datadir
