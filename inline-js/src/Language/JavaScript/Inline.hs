{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JavaScript.Inline
  ( expr,
    block,
    module Language.JavaScript.Inline.Core,
  )
where

import Data.ByteString.Builder
import Data.Coerce
import Data.List (nub)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.JavaScript.Inline.Class
import Language.JavaScript.Inline.Core
import Language.JavaScript.Parser.Lexer (Token (..), alexTestTokeniser)

qq :: (String -> Q TH.Exp) -> QuasiQuoter
qq myQQ =
  QuasiQuoter
    { quoteExp = myQQ,
      quotePat = error "Language.JavaScript.Inline: quotePat",
      quoteType = error "Language.JavaScript.Inline: quoteType",
      quoteDec = error "Language.JavaScript.Inline: quoteDec"
    }

-- | Produces expression splices with type @'FromEvalResult' a => 'JSSession' ->
-- 'IO' a@. Some things to keep in mind here:
--
-- 1. The spliced string should be a valid JavaScript expression. To write
--    control-flow statements, it's better to use 'block' instead.
--
-- 2. The spliced JavaScript expression is wrapped in an async arrow function,
--    and the returned @Promise@ is resolved to get the result to the Haskell
--    side. This means it's possible to use @await@ here. This applies to both
--    'expr' and 'block'.
--
-- 3. Use @$some_hs_var@ to refer to an in-scope Haskell variable. The
--    variable's type should be an 'ToJSCode' instance. 'ToJSCode' class is not
--    exposed, but it works for any 'Data.Aeson.ToJSON' instance, plus 'JSVal'
--    and several @bytestring@ types.
--
-- 4. Likewise, 'FromEvalResult' isn't exposed either, but it works for
--    'Data.Aeson.FromJSON' instances, 'JSVal's and @bytestring@ types. Explicit
--    type annotation of the returned value is often needed.
expr :: QuasiQuoter
expr = qq expressionQuasiQuoter

-- | Produces expression splices with the same type of 'expr'.
--
-- The spliced string should be a series of valid JavaScript statements.
-- JavaScript control-flow features (e.g. loops, try/catch) are supported. Use
-- @return@ to return the result.
--
-- The other rules of 'expr' also apply to 'block'.
block :: QuasiQuoter
block = qq blockQuasiQuoter

expressionQuasiQuoter :: String -> Q TH.Exp
expressionQuasiQuoter input = blockQuasiQuoter $ "return " <> input <> ";"

blockQuasiQuoter :: String -> Q TH.Exp
blockQuasiQuoter input =
  let tokens = either error id $ alexTestTokeniser input
      antiquotedParameterNames =
        nub [name | IdentifierToken {tokenLiteral = ('$' : name)} <- tokens]
      wrappedCode = wrapCode input antiquotedParameterNames
   in [|
        \session ->
          withFromEvalResult $ \p f -> do
            result <- eval session $ JSCode ($(wrappedCode) p)
            either fail pure (f result)
        |]

-- |
-- This fits the quasiquoted content into following format:
--
-- (async (a, b, c) => {return a + b + c;})(1, 2, 3)
--
-- where the argumentList is "a, b, c", the argumentValues are "1, 2, 3" and the
-- code is "a + b + c"
wrapCode :: String -> [String] -> Q TH.Exp
wrapCode code antiquotedNames =
  [|
    \p ->
      mconcat
        [ string7 "(async ( ",
          $(argumentList antiquotedNames),
          string7 " ) => { ",
          stringUtf8 code,
          string7 " })( ",
          $(argumentValues antiquotedNames),
          string7 " ).then(",
          coerce p,
          string7 ")"
        ]
    |]

argumentList :: [String] -> Q TH.Exp
argumentList rawNames =
  case rawNames of
    [] -> [|string7 ""|]
    (firstName : names) ->
      foldr
        (\name acc -> [|$(acc) <> string7 ", $" <> string7 name|])
        [|string7 ('$' : firstName)|]
        names

argumentValues :: [String] -> Q TH.Exp
argumentValues rawNames =
  case rawNames of
    [] -> [|string7 ""|]
    (firstName : names) ->
      foldr
        ( \name acc ->
            [|
              $(acc)
                <> string7 ", "
                <> coerce (toJSCode $(TH.varE $ TH.mkName name))
              |]
        )
        [|coerce (toJSCode $(TH.varE $ TH.mkName firstName))|]
        names
