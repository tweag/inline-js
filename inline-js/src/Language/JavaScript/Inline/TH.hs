{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.TH (js) where

import Data.List
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.JavaScript.Inline.Core
import Language.JavaScript.Inline.JSParse

-- | Generate a 'JSExpr' from inline JavaScript code. The code should be a
-- single expression or a code block with potentially multiple statements (use
-- @return@ to specify the result value in which case). Top-level @await@ is
-- supported.
--
-- Use @$var@ to refer to a Haskell variable @var@. @var@ should be an instance
-- of 'ToJS'.
--
-- Important: when using 'js', GHC calls the @node@ process at compile-time in
-- order to use a JavaScript-based JavaScript parser to extract necessary info.
-- Don't forget to ensure @node@ is available in @PATH@ at compile-time.
js :: QuasiQuoter
js = fromQuoteExp inlineJS

fromQuoteExp :: (String -> Q Exp) -> QuasiQuoter
fromQuoteExp q =
  QuasiQuoter
    { quoteExp = q,
      quotePat = error "Language.JavaScript.Inline.TH: quotePat",
      quoteType = error "Language.JavaScript.Inline.TH: quoteType",
      quoteDec = error "Language.JavaScript.Inline.TH: quoteDec"
    }

inlineJS :: String -> Q Exp
inlineJS js_code =
  do
    (is_sync, is_expr, hs_vars) <- runIO $ jsParse js_code
    [|
      mconcat
        $( listE
             ( [ [|
                   fromString
                     $( litE
                          ( stringL
                              ( ( if is_sync
                                    then "(("
                                    else "(async ("
                                )
                                  <> intercalate
                                    ","
                                    ['$' : v | v <- hs_vars]
                                  <> ") => {"
                                  <> ( if is_expr
                                         then
                                           "return "
                                             <> js_code
                                             <> ";"
                                         else js_code
                                     )
                                  <> "})("
                              )
                          )
                      )
                   |]
               ]
                 <> intersperse
                   [|fromString ","|]
                   [ [|toJS $(varE (mkName v))|]
                     | v <- hs_vars
                   ]
                 <> [[|fromString ")"|]]
             )
         )
      |]
