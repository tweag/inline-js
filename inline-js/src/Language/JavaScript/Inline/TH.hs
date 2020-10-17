{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline.TH where

import Data.List
import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.JavaScript.Inline.Core
import Language.JavaScript.Inline.JSParse

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
exprQuoter = asdf False

blockQuoter :: String -> Q Exp
blockQuoter = asdf False

exprAsyncQuoter :: String -> Q Exp
exprAsyncQuoter = asdf True

blockAsyncQuoter :: String -> Q Exp
blockAsyncQuoter = asdf True

asdf :: Bool -> String -> Q Exp
asdf is_async js_code = do
  (is_expr, hs_vars) <-
    case jsParse js_code of
      Left err -> fail err
      Right r -> pure r
  [|
    mconcat
      $( listE
           ( [ [|
                 fromString
                   $( litE
                        ( stringL
                            ( ( if is_async
                                  then "(async ("
                                  else "(("
                              )
                                <> intercalate "," ['$' : v | v <- hs_vars]
                                <> ") => {"
                                <> ( if is_expr
                                       then "return " <> js_code <> ";"
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
                 [[|toJS $(varE (mkName v))|] | v <- hs_vars]
               <> [[|fromString ")"|]]
           )
       )
    |]
