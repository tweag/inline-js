{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.TH where

import Data.Foldable
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.JavaScript.Inline.Class
import Language.JavaScript.Inline.Core
import Language.JavaScript.Parser.Lexer

expr :: QuasiQuoter
expr =
  QuasiQuoter
    { quoteExp = exprQuoter,
      quotePat = error "Language.JavaScript.Inline.TH: quotePat",
      quoteType = error "Language.JavaScript.Inline.TH: quoteType",
      quoteDec = error "Language.JavaScript.Inline.TH: quoteDec"
    }

block :: QuasiQuoter
block =
  QuasiQuoter
    { quoteExp = blockQuoter,
      quotePat = error "Language.JavaScript.Inline.TH: quotePat",
      quoteType = error "Language.JavaScript.Inline.TH: quoteType",
      quoteDec = error "Language.JavaScript.Inline.TH: quoteDec"
    }

exprQuoter :: String -> Q Exp
exprQuoter js_code = blockQuoter $ "return " <> js_code <> ";"

blockQuoter :: String -> Q Exp
blockQuoter js_code = do
  tokens <- case alexTestTokeniser js_code of
    Left err -> fail err
    Right tokens -> pure tokens
  let vars = nub [var | IdentifierToken {tokenLiteral = '$' : var} <- tokens]
      js_code_header =
        foldr'
          (\m0 m1 -> [|$(m0) <> $(m1)|])
          [|code ""|]
          [ [|code $(litE $ stringL $ "const $" <> var <> " = ") <> toJSCode $(varE $ mkName var) <> code "; "|]
            | var <- vars
          ]
  [|code "(async () => { " <> $(js_code_header) <> code $(litE $ stringL $ js_code <> " })()")|]
