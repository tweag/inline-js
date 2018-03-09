{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Exception
import Data.Aeson
import Language.JavaScript.Inline

main :: IO ()
main = do
  s <- newSession $(configureOptionsQ)
  flip finally (closeSession s) $ do
    nul <- eval s "let answer = 6 * 7"
    print (nul :: Value)
    answer <- eval s "answer"
    print (answer :: Double)
    tmpdir <- eval s "require('os').tmpdir()"
    print (tmpdir :: String)
    async <-
      eval
        s
        [js|new Promise((resolve, reject) => resolve('the answer is: ' + {{answer}}))|]
    print (async :: String)
