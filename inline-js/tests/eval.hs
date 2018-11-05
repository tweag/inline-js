{-# LANGUAGE OverloadedStrings #-}

import Language.JavaScript.Inline.Message
import Language.JavaScript.Inline.Session

main :: IO ()
main =
  withJSSession defJSSessionOpts $ \s -> do
    _ids <-
      traverse
        (sendMsg s)
        [ Eval "while(true){}" $ Just 1000
        , Eval "BOOM" Nothing
        , Eval "undefined" Nothing
        , Eval "let x = 6*7" Nothing
        , Eval "x" Nothing
        , EvalAsync "Promise.reject('BOOM')" Nothing Nothing
        , EvalAsync "Promise.resolve(x)" Nothing Nothing
        , EvalAsync
            "new Promise((resolve, _) => setTimeout(resolve, 10000))"
            Nothing
            (Just 1000)
        ]
    _rs <- traverse (recvMsg s) _ids
    print _rs
