{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Puppeteer
  ( PuppeteerPackage(..)
  , installPuppeteer
  , PuppeteerLaunchOpts(..)
  , PuppeteerOpts(..)
  , defPuppeteerOpts
  , Puppeteer
  , newPuppeteer
  , userAgent
  ) where

import Control.Monad.Fail
import qualified Data.Text as Text
import Data.Text.Lazy.Builder
import Language.JavaScript.Inline.Command
import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.JSON
import Language.JavaScript.Inline.Session
import qualified Paths_inline_js
import Prelude hiding (fail)
import System.Exit
import System.FilePath
import System.Process

data PuppeteerPackage
  = Core
  | Full
  deriving (Show)

pkgName :: PuppeteerPackage -> String
pkgName Core = "puppeteer-core"
pkgName Full = "puppeteer"

installPuppeteer :: PuppeteerPackage -> IO ()
installPuppeteer pkg = do
  _data_dir <- Paths_inline_js.getDataDir
  withCreateProcess
    (shell $ "npm install " <> pkgName pkg)
      {cwd = Just $ _data_dir </> "jsbits"} $ \_ _ _ ph -> do
    ec <- waitForProcess ph
    case ec of
      ExitSuccess -> pure ()
      _ ->
        fail $
        "Language.JavaScript.Inline.Puppeteer.installPuppeteer " <> show pkg <>
        " failed with " <>
        show ec

data PuppeteerLaunchOpts
  = PuppeteerLaunch { executablePath :: Maybe FilePath
                    , args :: [String] }
  | PuppeteerConnect { browserWSEndpoint :: Text.Text }
  deriving (Show)

data PuppeteerOpts = PuppeteerOpts
  { package :: PuppeteerPackage
  , launchOpts :: PuppeteerLaunchOpts
  } deriving (Show)

defPuppeteerOpts :: PuppeteerOpts
defPuppeteerOpts =
  PuppeteerOpts
    { package = Full
    , launchOpts =
        PuppeteerLaunch {executablePath = Nothing, args = ["--no-sandbox"]}
    }

data Puppeteer = Puppeteer
  { jsSession :: JSSession
  , puppeteerRegion :: JSRefRegion
  , puppeteerRef :: JSRef
  , browser :: JSCode
  }

newPuppeteer :: JSSession -> PuppeteerOpts -> IO Puppeteer
newPuppeteer s PuppeteerOpts {..} = do
  _region <- evalTo parseJSRefRegion s newJSRefRegion
  _ref <-
    evalAsyncJSRef s _region $
    asyncify $
    "await (require(\"" <> fromString (pkgName package) <> "\")" <>
    (case launchOpts of
       PuppeteerLaunch {..} ->
         ".launch(" <>
         codeFromValue
           (Object $
            maybe
              mempty
              (\p -> [("executablePath", String $ Text.pack p)])
              executablePath <>
            [("args", Array $ map (String . Text.pack) args)]) <>
         ")"
       PuppeteerConnect {..} ->
         ".connect(" <>
         codeFromValue
           (Object [("browserWSEndpoint", String browserWSEndpoint)]) <>
         ")") <>
    ")"
  pure
    Puppeteer
      { jsSession = s
      , puppeteerRegion = _region
      , puppeteerRef = _ref
      , browser = deRefJSRef _region _ref
      }

userAgent :: Puppeteer -> IO Text.Text
userAgent Puppeteer {..} =
  evalAsyncTo
    (\case
       String r -> Right r
       err ->
         Left $
         "Language.Inline.JavaScript.Puppeteer.userAgent received " <> show err)
    jsSession $
  browser <> ".version()"
