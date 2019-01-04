{-# LANGUAGE TemplateHaskell #-}

module Language.JavaScript.Inline
  ( js
  ) where

import qualified Data.Text as Text
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (liftString)
import Language.JavaScript.Inline.Command (eval)
import Language.JavaScript.Inline.JSCode (codeFromString)
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

-- JSCode -> JSSession -> IO JSCode
expQuasiQuoter :: String -> Q TH.Exp
expQuasiQuoter input =
  let tokens = either error id $ alexTestTokeniser input
      antiquotedParameterNames =
        [name | IdentifierToken {tokenLiteral = ('$':name)} <- tokens]
   in do [|\session -> do
             void $
               $(evaluateAll $
                 fmap nameToJSVarDeclaration $ antiquotedParameterNames)
             eval session (codeFromString $ Text.pack input)|]

-- [JSCode] -> IO ()
evaluateAll :: [Q TH.Exp] -> Q TH.Exp
evaluateAll = foldr (\dec acc -> [|$(acc) >> eval session $(dec)|]) [|pure ()|]

-- String -> JSCode
nameToJSVarDeclaration :: String -> Q TH.Exp
nameToJSVarDeclaration name =
  let variableReference :: Q TH.Exp
      variableReference = TH.varE $ TH.mkName name
   in [|codeFromString $
        "let $" <> $(liftString name) <> " = " <>
        encodeText $(variableReference) <>
        ";"|]
