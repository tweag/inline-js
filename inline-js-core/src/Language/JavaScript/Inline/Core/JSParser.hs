{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.Core.JSParser where

import Control.Exception
import qualified Data.ByteString as BS
import Data.Foldable
import Distribution.Simple.Utils
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.JavaScript.Inline.Core.Session
import Language.JavaScript.Inline.Core.Utils
import System.Directory
import System.Exit
import System.FilePath
import System.Process

data JSParserState = JSParserState
  { jsParserTmpDir :: FilePath,
    jsParserSession :: Session
  }

getJSParserState :: Q JSParserState
getJSParserState = do
  m <- getQ
  case m of
    Just r -> pure r
    _ -> do
      r <-
        runIO $ do
          tmpdir <- getTemporaryDirectory
          dir <- createTempDirectory tmpdir "inline-js"
          ( do
              for_ babelParser $ \(k, v) -> do
                let p = dir </> k
                createDirectoryIfMissing True (takeDirectory p)
                BS.writeFile p v
              s <- newSession defaultConfig {nodeModules = Just dir}
              pure JSParserState {jsParserTmpDir = dir, jsParserSession = s}
            )
            `onException` removePathForcibly dir
      addModFinalizer $
        runIO $ do
          closeSession (jsParserSession r)
          removePathForcibly (jsParserTmpDir r)
      putQ r
      pure r

babelParser :: [(FilePath, BS.ByteString)]
babelParser =
  $( do
       tmpdir <-
         runIO $ do
           tmpdir <- getTemporaryDirectory
           r <- createTempDirectory tmpdir "inline-js"
           ( do
               BS.writeFile
                 (r </> "package.json")
                 $(embedFile ("jsbits" </> "package.json"))
               BS.writeFile
                 (r </> "package-lock.json")
                 $(embedFile ("jsbits" </> "package-lock.json"))
               (ec, _, _) <-
                 readCreateProcessWithExitCode
                   ((shell "npm install") {cwd = Just r})
                   ""
               case ec of
                 ExitSuccess -> pure r
                 _ -> fail "npm install failed"
             )
             `onException` removePathForcibly r
       r <- embedDir tmpdir
       runIO $ removePathForcibly tmpdir
       pure r
   )
