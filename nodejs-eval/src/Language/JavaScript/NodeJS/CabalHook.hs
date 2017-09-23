{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.NodeJS.CabalHook
  ( defaultMainWithEvalServer
  , defaultUserHooksWithEvalServer
  , withEvalServer
  , addDependencies
  ) where

import Data.Aeson
import Data.Binary (encodeFile)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Language.JavaScript.NodeJS.CabalHook.Splices
import System.Directory
import System.Exit
import System.FilePath
import System.Process

defaultMainWithEvalServer :: (Value -> IO Value) -> IO ()
defaultMainWithEvalServer =
  defaultMainWithHooks . defaultUserHooksWithEvalServer

defaultUserHooksWithEvalServer :: (Value -> IO Value) -> UserHooks
defaultUserHooksWithEvalServer f = withEvalServer f simpleUserHooks

withEvalServer :: (Value -> IO Value) -> UserHooks -> UserHooks
withEvalServer f hooks =
  hooks
  { postConf =
      \args flags pkg_descr lbi -> do
        postConf hooks args flags pkg_descr lbi
        encodeFile ".buildinfo" $
          datadir (absoluteInstallDirs pkg_descr lbi NoCopyDest)
        let src_datadir = $(datadirQ)
        let target_datadir =
              datadir (absoluteInstallDirs pkg_descr lbi NoCopyDest)
        for_ ["jsbits" </> fn | fn <- ["package.json", "server.js"]] $ \p -> do
          createDirectoryIfMissing True $ takeDirectory $ target_datadir </> p
          copyFile (src_datadir </> p) (target_datadir </> p)
        b <- LBS.readFile $ target_datadir </> "jsbits" </> "package.json"
        v <-
          case eitherDecode' b of
            Left err -> fail err
            Right v -> pure v
        v' <- f v
        LBS.writeFile (target_datadir </> "jsbits" </> "package.json") $
          encode v'
        withCreateProcess
          ((shell "npm install") {cwd = Just $ target_datadir </> "jsbits"}) $ \_ _ _ h -> do
          r <- waitForProcess h
          case r of
            ExitSuccess -> pure ()
            ExitFailure x ->
              fail $ "npm install failed with exit code " ++ show x
  }

addDependencies :: [(T.Text, T.Text)] -> Value -> IO Value
addDependencies name_vers (Object obj) =
  case HM.lookup "dependencies" obj of
    Just (Object obj') ->
      pure $
      Object $
      HM.insert
        "dependencies"
        (Object $
         HM.union
           (HM.fromList [(name, String ver) | (name, ver) <- name_vers])
           obj')
        obj
    _ -> fail "dependencies field not present"
addDependencies _ _ = fail "invalid package.json"
