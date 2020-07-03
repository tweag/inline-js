{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.TH where

import Language.Haskell.TH
import Language.JavaScript.Inline.Class
import Language.JavaScript.Inline.Core
import Language.JavaScript.Parser.Lexer

exprQuoter :: String -> Q Exp
exprQuoter raw_js_code = do
  js_tokens <- case alexTestTokeniser raw_js_code of
    Left err -> fail err
    Right tokens -> pure tokens
  foldr1
    (\m0 m1 -> [|$(m0) <> $(m1)|])
    [ case token of
        IdentifierToken {tokenLiteral = '$' : hs_var} -> [|toJSCode $(varE $ mkName hs_var)|]
        _ -> [|code $(litE $ stringL $ tokenLiteral token)|]
      | token <- js_tokens
    ]
