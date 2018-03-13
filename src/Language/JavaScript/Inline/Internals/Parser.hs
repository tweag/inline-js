{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.Internals.Parser
  ( Chunk(..)
  , Parser
  , parseChunks
  ) where

import Data.Functor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Chunk
  = LitChunk String
  | QuotedChunk String
  deriving (Show)

type Parser = Parsec Void String

parseQuotedChunk :: Parser String
parseQuotedChunk = do
  void $ char '$'
  c <- between (char '(') (char ')') $ takeWhile1P Nothing (/= ')')
  pure c

parseChunks :: Parser [Chunk]
parseChunks = do
  l <- w
  pure $ foldr f [] l
  where
    w =
      (do x <- try (Right <$> parseQuotedChunk) <|> Left <$> anyChar
          cs <- w
          pure $ x : cs) <|>
      pure []
    f (Left c) (LitChunk c':cs) = LitChunk (c : c') : cs
    f (Left c) cs = LitChunk [c] : cs
    f (Right c) cs = QuotedChunk c : cs
