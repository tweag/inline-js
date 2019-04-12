{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JavaScript.Inline
  ( expr
  , block
  , newJSSession
  , withJSSession
  , closeJSSession
  , defJSSessionOpts
  , JSSessionOpts(..)
  , JSSession
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub)
import Data.Text (pack)
import qualified Data.Text.Encoding as Text
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.JavaScript.Inline.Command (eval)
import Language.JavaScript.Inline.JSCode (codeFromString)
import Language.JavaScript.Inline.Session
  ( JSSession
  , JSSessionOpts(..)
  , closeJSSession
  , defJSSessionOpts
  , newJSSession
  , withJSSession
  )
import Language.JavaScript.Parser.Lexer (Token(..), alexTestTokeniser)

qq :: (String -> Q TH.Exp) -> QuasiQuoter
qq myQQ =
  QuasiQuoter
    { quoteExp = myQQ
    , quotePat = error "Language.JavaScript.Inline: quotePat"
    , quoteType = error "Language.JavaScript.Inline: quoteType"
    , quoteDec = error "Language.JavaScript.Inline: quoteDec"
    }

-- | Produces splice with type @'Aeson.FromJSON' a => 'JSSession' -> 'IO' a@
expr :: QuasiQuoter
expr = qq expressionQuasiQuoter

-- | Produces splice with the same type of 'expr'
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
          result <- eval session $ codeFromString $(wrappedCode)
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
      [ pack "(async ( "
      , $(argumentList antiquotedNames)
      , pack " ) => { "
      , pack code
      , pack " })( "
      , $(argumentValues antiquotedNames)
      , pack " ).then(r => JSON.stringify(r))"
      ]|]

argumentList :: [String] -> Q TH.Exp
argumentList rawNames =
  case rawNames of
    [] -> [|pack ""|]
    (firstName:names) ->
      foldr
        (\name acc -> [|$(acc) <> pack ", $" <> pack name|])
        [|pack ('$' : firstName)|]
        names

argumentValues :: [String] -> Q TH.Exp
argumentValues rawNames =
  case rawNames of
    [] -> [|pack ""|]
    (firstName:names) ->
      foldr
        (\name acc ->
           [|$(acc) <> pack ", " <>
             Text.decodeUtf8
               (LBS.toStrict $ Aeson.encode $(TH.varE $ TH.mkName name))|])
        [|Text.decodeUtf8
            (LBS.toStrict $ Aeson.encode $(TH.varE $ TH.mkName firstName))|]
        names
