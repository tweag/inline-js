{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.JavaScript.Inline.Configure
  ( ConfigureOptions(..)
  , defaultConfigureOptions
  , configureOptionsQ
  , defaultMainWithInlineJS
  , defaultUserHooksWithInlineJS
  , withInlineJS
  ) where

import Control.Monad
import Data.Binary
import Data.Foldable
import Data.List.Extra
import Data.Version
import Distribution.Simple hiding (Version)
import Distribution.Simple.LocalBuildInfo
import GHC.Generics
import Language.Haskell.TH.Syntax
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

deriving instance Lift Version

data ConfigureOptions = ConfigureOptions
  { jsbitsPath :: FilePath
  , commands :: [String]
  , minNodeVer :: Version
  , nodeArgs :: [String]
  , additionalEnv :: [(String, String)]
  } deriving (Show, Generic, Binary, Lift)

defaultConfigureOptions :: ConfigureOptions
defaultConfigureOptions =
  ConfigureOptions
    { jsbitsPath = ""
    , commands = ["npm install"]
    , minNodeVer = makeVersion [6]
    , nodeArgs = ["server.js"]
    , additionalEnv = []
    }

configureOptionsQ :: Q Exp
configureOptionsQ = do
  (conf_opts :: ConfigureOptions) <- runIO $ decodeFile ".buildinfo"
  lift conf_opts

defaultMainWithInlineJS :: ConfigureOptions -> IO ()
defaultMainWithInlineJS = defaultMainWithHooks . defaultUserHooksWithInlineJS

defaultUserHooksWithInlineJS :: ConfigureOptions -> UserHooks
defaultUserHooksWithInlineJS opts = withInlineJS opts simpleUserHooks

withInlineJS :: ConfigureOptions -> UserHooks -> UserHooks
withInlineJS conf_opts@ConfigureOptions {..} hooks =
  hooks
    { postConf =
        \args flags pkg_descr lbi -> do
          nv <- readProcess "node" ["--version"] ""
          case nv of
            'v':nv' ->
              let v = makeVersion $ map read $ splitOn "." nv'
               in unless (minNodeVer <= v) $
                  fail $
                  "node version is " ++
                  show v ++
                  ", but minimum required version is " ++ show minNodeVer
            _ -> fail $ "unrecognized output from node --version: " ++ nv
          let src_datadir = $(runIO (decodeFile ".buildinfo") >>= liftString)
              src_jsbits = src_datadir </> "jsbits"
              target_datadir =
                datadir (absoluteInstallDirs pkg_descr lbi NoCopyDest)
              target_jsbits = target_datadir </> "jsbits"
          createDirectoryIfMissing True target_jsbits
          for_ ["package.json", "server.js"] $ \p ->
            copyFile (src_jsbits </> p) (target_jsbits </> p)
          current_env <- getEnvironment
          for_ commands $ \command ->
            withCreateProcess
              ((shell command)
                 { cwd = Just target_jsbits
                 , env = Just $ additionalEnv ++ current_env
                 }) $ \_ _ _ h -> do
              r <- waitForProcess h
              case r of
                ExitSuccess -> pure ()
                ExitFailure x ->
                  fail $
                  show command ++ " failed with exit code " ++ show x
          encodeFile ".buildinfo" conf_opts {jsbitsPath = target_jsbits}
          postConf hooks args flags pkg_descr lbi
    }
