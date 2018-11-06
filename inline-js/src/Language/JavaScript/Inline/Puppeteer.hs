{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Puppeteer
  ( installPuppeteer
  , PuppeteerOpts(..)
  , getDefPuppeteerOpts
  , Puppeteer
  , newPuppeteer
  , userAgent
  ) where

import Control.Monad.Fail
import qualified Data.Text as Text
import Language.JavaScript.Inline.Command
import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.JSON
import Language.JavaScript.Inline.Session
import qualified Paths_inline_js
import Prelude hiding (fail)
import System.Directory
import System.Exit
import System.FilePath
import System.Process

installPuppeteer :: IO ()
installPuppeteer = do
  _data_dir <- Paths_inline_js.getDataDir
  withCreateProcess
    (shell "npm install puppeteer-core") {cwd = Just $ _data_dir </> "jsbits"} $ \_ _ _ ph -> do
    ec <- waitForProcess ph
    case ec of
      ExitSuccess -> pure ()
      _ ->
        fail $
        "Language.JavaScript.Inline.Puppeteer.installPuppeteer failed with " <>
        show ec

data PuppeteerOpts = PuppeteerOpts
  { executablePath :: FilePath
  , args :: [String]
  } deriving (Show)

getDefPuppeteerOpts :: IO PuppeteerOpts
getDefPuppeteerOpts = do
  Just _chromium <- findExecutable "chromium"
  pure PuppeteerOpts {executablePath = _chromium, args = ["--no-sandbox"]}

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
    "await (require(\"puppeteer-core\").launch(" <>
    codeFromValue
      (Object
         [ ("executablePath", String $ Text.pack executablePath)
         , ("args", Array $ map (String . Text.pack) args)
         ]) <>
    "))"
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
