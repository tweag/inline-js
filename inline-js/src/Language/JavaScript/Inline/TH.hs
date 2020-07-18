{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.TH where

import Data.Foldable
import Data.List
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.JavaScript.Inline.Core
import Language.JavaScript.Parser.Lexer

-- | Generate a 'JSExpr' from an inline JavaScript expression. Use @$var@ to
-- refer to a Haskell variable @var@ (its type should be an 'ToJS' instance).
expr :: QuasiQuoter
expr = fromQuoteExp exprQuoter

-- | Generate a 'JSExpr' from an inline JavaScript code block. Use @return@ in
-- the code block to return the result. Other rules of 'expr' also applies here.
block :: QuasiQuoter
block = fromQuoteExp blockQuoter

-- | Like 'expr', but supports @await@.
exprAsync :: QuasiQuoter
exprAsync = fromQuoteExp exprAsyncQuoter

-- | Like 'block', but supports @await@.
blockAsync :: QuasiQuoter
blockAsync = fromQuoteExp blockAsyncQuoter

fromQuoteExp :: (String -> Q Exp) -> QuasiQuoter
fromQuoteExp q =
  QuasiQuoter
    { quoteExp = q,
      quotePat = error "Language.JavaScript.Inline.TH: quotePat",
      quoteType = error "Language.JavaScript.Inline.TH: quoteType",
      quoteDec = error "Language.JavaScript.Inline.TH: quoteDec"
    }

exprQuoter :: String -> Q Exp
exprQuoter js_code = blockQuoter $ "return " <> js_code <> ";"

blockQuoter :: String -> Q Exp
blockQuoter js_code =
  [|fromString "(() => { " <> $(jsCodeHeader js_code) <> fromString $(litE $ stringL $ js_code <> " })()")|]

exprAsyncQuoter :: String -> Q Exp
exprAsyncQuoter js_code = blockAsyncQuoter $ "return " <> js_code <> ";"

blockAsyncQuoter :: String -> Q Exp
blockAsyncQuoter js_code =
  [|fromString "(async () => { " <> $(jsCodeHeader js_code) <> fromString $(litE $ stringL $ js_code <> " })()")|]

jsCodeHeader :: String -> Q Exp
jsCodeHeader js_code = do
  tokens <- case alexTestTokeniser js_code of
    Left err -> fail err
    Right tokens -> pure tokens
  let vars = nub [var | IdentifierToken {tokenLiteral = '$' : var} <- tokens]
      js_code_header =
        foldr'
          (\m0 m1 -> [|$(m0) <> $(m1)|])
          [|(mempty :: JSExpr)|]
          [ [|fromString $(litE $ stringL $ "const $" <> var <> " = ") <> toJS $(varE $ mkName var) <> fromString "; "|]
            | var <- vars
          ]
  js_code_header
