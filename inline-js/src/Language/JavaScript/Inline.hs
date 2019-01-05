{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.JavaScript.Inline
  ( js
  ) where

import Data.List (nub)
import Data.Text (pack)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.JavaScript.Inline.Command (eval)
import Language.JavaScript.Inline.JSCode (codeFromString, codeToString)
import Language.JavaScript.Inline.JSON (encodeText)
import Language.JavaScript.Parser.Lexer (Token(..), alexTestTokeniser)

js :: QuasiQuoter
js =
  QuasiQuoter
    { quoteExp = expQuasiQuoter
    , quotePat = error "Language.JavaScript.Inline: quotePat"
    , quoteType = error "Language.JavaScript.Inline: quoteType"
    , quoteDec = error "Language.JavaScript.Inline: quoteDec"
    }

-- produces splice with type `JSSession -> IO JSCode`
expQuasiQuoter :: String -> Q TH.Exp
expQuasiQuoter input =
  let tokens = either error id $ alexTestTokeniser input
      antiquotedParameterNames =
        nub [name | IdentifierToken {tokenLiteral = ('$':name)} <- tokens]
      wrappedCode = wrapCode input antiquotedParameterNames
   in do [|\session -> do eval session $ codeFromString $(wrappedCode)|]

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
      , " ) { return "
      , codeToString code
      , "; })( "
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
           [|$(acc) <> ", " <> encodeText $(TH.varE $ TH.mkName name)|])
        [|encodeText $(TH.varE $ TH.mkName firstName)|]
        names
