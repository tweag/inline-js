{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Language.JavaScript.Inline.JSParse
  ( jsParse,
  )
where

import Data.Generics
  ( GenericQ,
    everything,
  )
import qualified Data.Set as S
import Language.JavaScript.Parser
import Language.JavaScript.Parser.Grammar7
import Language.JavaScript.Parser.Parser
import Type.Reflection

jsParse :: String -> Either String (Bool, [String])
jsParse src
  | Right ast <- parseUsing parseExpression src "src" = Right (True, hsVars ast)
  | otherwise = case parse src "src" of
    Left err -> Left err
    Right ast -> Right (False, hsVars ast)

hsVars :: GenericQ [String]
hsVars = S.toList . everything (<>) w
  where
    w :: GenericQ (S.Set String)
    w t
      | Just HRefl <- eqTypeRep (typeOf t) (typeRep @JSExpression) = case t of
        JSIdentifier _ ('$' : v) -> S.singleton v
        _ -> mempty
      | otherwise = mempty
