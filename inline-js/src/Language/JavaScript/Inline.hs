{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.JavaScript.Inline
  ( expr
  , block
  ) where

import Data.List (nub)
import Data.Text (pack)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.JavaScript.Inline.Command (eval)
import Language.JavaScript.Inline.JSCode (codeFromString, codeToString)
import Language.JavaScript.Inline.JSON (encodeText)
import qualified Language.JavaScript.Inline.JsonConvertible as JsonConvertible
import Language.JavaScript.Parser.Lexer (Token(..), alexTestTokeniser)

qq :: (String -> Q TH.Exp) -> QuasiQuoter
qq myQQ =
  QuasiQuoter
    { quoteExp = myQQ
    , quotePat = error "Language.JavaScript.Inline: quotePat"
    , quoteType = error "Language.JavaScript.Inline: quoteType"
    , quoteDec = error "Language.JavaScript.Inline: quoteDec"
    }

expr :: QuasiQuoter
expr = qq expressionQuasiQuoter

block :: QuasiQuoter
block = qq blockQuasiQuoter

--- produces splice with type `JSSession -> IO JSCode`
expressionQuasiQuoter :: String -> Q TH.Exp
expressionQuasiQuoter input = blockQuasiQuoter $ "return " ++ input ++ ";"

--- produces splice with type `JSSession -> IO JSCode`
blockQuasiQuoter :: String -> Q TH.Exp
blockQuasiQuoter input =
  let tokens = either error id $ alexTestTokeniser input
      antiquotedParameterNames =
        nub [name | IdentifierToken {tokenLiteral = ('$':name)} <- tokens]
      wrappedCode = wrapCode input antiquotedParameterNames
   in [|\session -> do
          result <- eval session $ codeFromString $(wrappedCode)
          return . JsonConvertible.parse $ encodeText result|]

--
-- This fits the quasiquoted content into following format:
--
-- (function(a, b, c) {
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
      [ "(function( "
      , $(argumentList antiquotedNames)
      , " ) { "
      , codeToString code
      , " })( "
      , $(argumentValues $ antiquotedNames)
      , " );"
      ]|]

argumentList :: [String] -> Q TH.Exp
argumentList rawNames =
  case rawNames of
    [] -> [|""|]
    (firstName:names) ->
      foldr
        (\name acc -> [|$(acc) <> ", $" <> pack name|])
        [|"$" <> pack firstName|]
        names

argumentValues :: [String] -> Q TH.Exp
argumentValues rawNames =
  case rawNames of
    [] -> [|""|]
    (firstName:names) ->
      foldr
        (\name acc ->
           [|$(acc) <> ", " <>
             JsonConvertible.stringify $(TH.varE $ TH.mkName name)|])
        [|JsonConvertible.stringify $(TH.varE $ TH.mkName firstName)|]
        names
