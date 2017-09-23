{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.NodeJS.Splices
  ( splice
  , typedSplice
  ) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Language.Haskell.TH.Syntax
import Language.JavaScript.NodeJS.CabalHook.Splices
import Network.HTTP.Simple
import Network.HTTP.Types
import System.FilePath
import System.Process

splice :: Q Exp
splice =
  [|do (_, _, _, h) <-
         createProcess
           ((proc "node" ["server.js", "--port", "23333"])
            {cwd = Just ($(datadirQ) </> "jsbits")})
       pure
         ( \code -> do
             initReq <- parseRequest "http://localhost:23333/eval"
             resp <-
               httpJSON $
               setRequestMethod methodPost $
               setRequestBodyJSON
                 (Object $ HM.singleton (T.pack "code") (String code))
                 initReq
             case getResponseBody resp of
               Object obj ->
                 case HM.lookup (T.pack "success") obj of
                   Just v -> pure v :: IO Value
                   _ -> fail $ "evaluation failed: " ++ show resp
               _ -> fail $ "illegal response: " ++ show resp
         , terminateProcess h)|]

typedSplice :: Q (TExp (IO (T.Text -> IO Value, IO ())))
typedSplice = unsafeTExpCoerce splice
