{-# LANGUAGE OverloadedStrings #-}

import Language.JavaScript.Inline.Message
import Language.JavaScript.Inline.Session

main :: IO ()
main =
  withJSSession defJSSessionOpts $ \s -> do
    _ids <-
      traverse
        (sendMsg s)
        [ Eval "while(true){}" (Just 1000) Nothing False
        , Eval "BOOM" Nothing Nothing False
        , Eval "undefined" Nothing Nothing False
        , Eval "let x = 6*7" Nothing Nothing False
        , Eval "x" Nothing Nothing False
        , Eval "\"left\" + \"pad\"" Nothing Nothing False
        , Eval "Promise.reject('BOOM')" Nothing Nothing True
        , Eval "Promise.resolve(x)" Nothing Nothing True
        , Eval
            "new Promise((resolve, _) => setTimeout(resolve, 10000))"
            Nothing
            (Just 1000)
            True
        ]
    _rs <- traverse (recvMsg s) _ids
    print _rs
