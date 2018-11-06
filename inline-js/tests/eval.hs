{-# LANGUAGE OverloadedStrings #-}

import Data.Foldable
import Language.JavaScript.Inline.JSCode
import Language.JavaScript.Inline.Message
import Language.JavaScript.Inline.Session

main :: IO ()
main =
  withJSSession defJSSessionOpts $ \s -> do
    _ids <-
      traverse
        (sendMsg s)
        [ Eval "while(true){}" (Just 1000) Nothing False Nothing
        , Eval "BOOM" Nothing Nothing False Nothing
        , Eval "undefined" Nothing Nothing False Nothing
        , Eval "let x = 6*7" Nothing Nothing False Nothing
        , Eval "x" Nothing Nothing False Nothing
        , Eval "\"left\" + \"pad\"" Nothing Nothing False Nothing
        , Eval "Promise.reject('BOOM')" Nothing Nothing True Nothing
        , Eval "Promise.resolve(x)" Nothing Nothing True Nothing
        , Eval
            "new Promise((resolve, _) => setTimeout(resolve, 10000))"
            Nothing
            (Just 1000)
            True
            Nothing
        , Eval (asyncify "x") Nothing Nothing True Nothing
        , Eval "require.toString()" Nothing Nothing True Nothing
        ]
    _rs <- traverse (recvMsg s) _ids
    traverse_ print _rs
