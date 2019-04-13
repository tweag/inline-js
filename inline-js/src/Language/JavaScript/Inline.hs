{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JavaScript.Inline
  ( expr
  , block
  , module Language.JavaScript.Inline.Core
  ) where

import qualified Data.Aeson as Aeson
import Data.ByteString.Builder
import Data.List (nub)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.JavaScript.Inline.Core
import Language.JavaScript.Parser.Lexer (Token(..), alexTestTokeniser)

qq :: (String -> Q TH.Exp) -> QuasiQuoter
qq myQQ =
  QuasiQuoter
    { quoteExp = myQQ
    , quotePat = error "Language.JavaScript.Inline: quotePat"
    , quoteType = error "Language.JavaScript.Inline: quoteType"
    , quoteDec = error "Language.JavaScript.Inline: quoteDec"
    }

-- | Produces expression splices with type @'Aeson.FromJSON' a => 'JSSession' -> 'IO' a@
--
-- The spliced string should be a valid JavaScript expression.
-- Shall you need to write control-flow statements, please use 'block' instead.
--
-- Note that it's possible to use @await@ in both 'expr' and 'block',
-- since the code is wrapped into an async arrow function under the hood.
--
-- Use @$some_hs_var@ to refer to an in-scope Haskell variable.
-- The variable's type should be an 'Aeson.ToJSON' instance.
expr :: QuasiQuoter
expr = qq expressionQuasiQuoter

-- | Produces expression splices with the same type of 'expr'.
--
-- The spliced string should be a series of valid JavaScript statements.
-- JavaScript control-flow features (e.g. loops, try/catch) are supported.
-- Use @return@ to return the result.
--
-- Most rules of 'expr' also apply to 'block'.
block :: QuasiQuoter
block = qq blockQuasiQuoter

expressionQuasiQuoter :: String -> Q TH.Exp
expressionQuasiQuoter input = blockQuasiQuoter $ "return " <> input <> ";"

blockQuasiQuoter :: String -> Q TH.Exp
blockQuasiQuoter input =
  let tokens = either error id $ alexTestTokeniser input
      antiquotedParameterNames =
        nub [name | IdentifierToken {tokenLiteral = ('$':name)} <- tokens]
      wrappedCode = wrapCode input antiquotedParameterNames
   in [|\session -> do
          result <- eval session $ JSCode $(wrappedCode)
          either fail pure $ Aeson.eitherDecode' result|]

-- |
-- This fits the quasiquoted content into following format:
--
-- (async (a, b, c) => {
--   return a + b + c;
-- })(1, 2, 3)
--
-- where the argumentList is "a, b, c",
-- the argumentValues are "1, 2, 3"
-- and the code is "a + b + c"
--
wrapCode :: String -> [String] -> Q TH.Exp
wrapCode code antiquotedNames =
  [|mconcat
      [ string7 "(async ( "
      , $(argumentList antiquotedNames)
      , string7 " ) => { "
      , stringUtf8 code
      , string7 " })( "
      , $(argumentValues antiquotedNames)
      , string7 " ).then(r => JSON.stringify(r))"
      ]|]

argumentList :: [String] -> Q TH.Exp
argumentList rawNames =
  case rawNames of
    [] -> [|string7 ""|]
    (firstName:names) ->
      foldr
        (\name acc -> [|$(acc) <> string7 ", $" <> string7 name|])
        [|string7 ('$' : firstName)|]
        names

argumentValues :: [String] -> Q TH.Exp
argumentValues rawNames =
  case rawNames of
    [] -> [|string7 ""|]
    (firstName:names) ->
      foldr
        (\name acc ->
           [|$(acc) <> string7 ", " <>
             lazyByteString (Aeson.encode $(TH.varE $ TH.mkName name))|])
        [|lazyByteString (Aeson.encode $(TH.varE $ TH.mkName firstName))|]
        names
