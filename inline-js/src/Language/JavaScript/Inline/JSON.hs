{-# LANGUAGE StrictData #-}

module Language.JavaScript.Inline.JSON
  ( Value(..)
  , JSString
  , Object
  , Array
  , encode
  , encodeLBS
  , decode
  ) where

import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Binary.Get
import Data.Bits
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Internal.Encoding.Utf8 as Text
import Prelude hiding (fail)

data Value
  = Object Object
  | Array Array
  | String JSString
  | Number Double
  | Bool Bool
  | Null
  deriving (Eq, Show)

type JSString = Text.Text

type Object = Map.Map JSString Value

type Array = [Value]

encodeChar :: Char -> Builder
encodeChar c
  | c <= '\x1F' || c == '"' || c == '\\' =
    string7 "\\u" <> word16HexFixed (fromIntegral (ord c))
  | otherwise = charUtf8 c

encodeString :: JSString -> Builder
encodeString s =
  char7 '"' <> Text.foldl' (\b c -> b <> encodeChar c) mempty s <> char7 '"'

encode :: Value -> Builder
encode v =
  case v of
    Object o ->
      char7 '{' <>
      mconcat
        (intersperse
           (char7 ',')
           [encodeString k <> char7 ':' <> encode v' | (k, v') <- Map.toList o]) <>
      char7 '}'
    Array l ->
      char7 '[' <> mconcat (intersperse (char7 ',') (map encode l)) <> char7 ']'
    String s -> encodeString s
    Number x -> doubleDec x
    Bool False -> string7 "false"
    Bool True -> string7 "true"
    Null -> string7 "null"

encodeLBS :: Value -> LBS.ByteString
encodeLBS = toLazyByteString . encode

anyChar :: Get Char
anyChar = do
  b1 <- getWord8
  if Text.validate1 b1
    then pure (chr (fromIntegral b1))
    else do
      b2 <- getWord8
      if Text.validate2 b1 b2
        then pure (Text.chr2 b1 b2)
        else do
          b3 <- getWord8
          if Text.validate3 b1 b2 b3
            then pure (Text.chr3 b1 b2 b3)
            else do
              b4 <- getWord8
              if Text.validate4 b1 b2 b3 b4
                then pure (Text.chr4 b1 b2 b3 b4)
                else fail $
                     "Language.JavaScript.Inline.JSON.anyChar: " <>
                     show (b1, b2, b3, b4)

satisfy :: (Char -> Bool) -> Get Char
satisfy f = do
  c <- anyChar
  if f c
    then pure c
    else fail $ "Language.JavaScript.Inline.JSON.satisfy: " <> show c

satisfy' :: (Char -> Bool) -> Get ()
satisfy' f = do
  c <- anyChar
  unless (f c) $ fail $ "Language.JavaScript.Inline.JSON.satisfy': " <> show c

char :: Char -> Get Char
char = satisfy . (==)

char' :: Char -> Get ()
char' = satisfy' . (==)

string :: String -> Get ()
string = traverse_ char'

spaceChar :: Get ()
spaceChar = satisfy' (`elem` ['\x09', '\x0A', '\x0D', '\x20'])

space :: Get ()
space = void $ many spaceChar

lexeme :: Get a -> Get a
lexeme = (space *>)

lexemeChar :: Char -> Get ()
lexemeChar = lexeme . char'

lexemeString :: String -> Get ()
lexemeString = lexeme . string

sepBy :: Get s -> Get a -> Get [a]
sepBy s g = ((:) <$> g <*> many (s *> g)) <|> pure []

sepByComma :: Get a -> Get [a]
sepByComma = sepBy (lexemeChar ',')

bracket :: Get s -> Get s -> Get a -> Get a
bracket l r g = l *> g <* r

hex16 :: Get Int
hex16 = do
  s <- replicateM 4 $ satisfy isHexDigit
  pure $ foldl' (\tot c -> (tot `shiftL` 4) .|. digitToInt c) 0 s

textChar :: Get Char
textChar = do
  c <- anyChar
  case c of
    '"' -> fail "Language.JavaScript.Inline.JSON.textChar: encountered '\"'"
    '\\' -> do
      c' <- anyChar
      case c' of
        '"' -> pure '"'
        '\\' -> pure '\\'
        '/' -> pure '/'
        'b' -> pure '\b'
        'f' -> pure '\f'
        'n' -> pure '\n'
        'r' -> pure '\r'
        't' -> pure '\t'
        'u' -> chr <$> hex16
        _ ->
          fail $
          "Language.JavaScript.Inline.JSON.textChar: invalid escape character: " <>
          show [c']
    _
      | c <= '\x1F' ->
        fail $
        "Language.JavaScript.Inline.JSON.textChar: encountered control character: " <>
        show [c]
      | otherwise -> pure c

text :: Get JSString
text = bracket (char' '"') (char' '"') (Text.pack <$> many textChar)

lexemeText :: Get JSString
lexemeText = lexeme text

nullableString :: Get String -> Get String
nullableString = (<|> pure "")

octString :: Get String
octString = some $ satisfy isDigit

number :: Get Double
number = do
  s <-
    do _sign <- nullableString $ char' '-' $> "-"
       _int <- octString
       _frac <- nullableString $ (:) <$> char '.' <*> octString
       _exp <-
         nullableString $
         (\e sgn n -> e : (sgn <> n)) <$> satisfy ((== 'e') . toLower) <*>
         nullableString ((: []) <$> satisfy (`elem` ['+', '-'])) <*>
         octString
       pure $ _sign <> _int <> _frac <> _exp
  case reads s of
    [(r, "")] -> pure r
    rs ->
      fail $
      "Language.JavaScript.Inline.JSON.number: parsing failed, result: " <>
      show rs

lexemeNumber :: Get Double
lexemeNumber = lexeme number

lexemeValue :: Get Value
lexemeValue =
  bracket
    (lexemeChar '{')
    (lexemeChar '}')
    (Object . Map.fromList <$>
     sepByComma ((,) <$> lexemeText <*> (lexemeChar ':' *> lexemeValue))) <|>
  bracket (lexemeChar '[') (lexemeChar ']') (Array <$> sepByComma lexemeValue) <|>
  (String <$> lexemeText) <|>
  (Number <$> lexemeNumber) <|>
  (Bool False <$ lexemeString "false") <|>
  (Bool True <$ lexemeString "true") <|>
  (Null <$ lexemeString "null")

decode :: LBS.ByteString -> Either String Value
decode s =
  case runGetOrFail (lexemeValue <* space) s of
    Right (rest, _, r)
      | LBS.null rest -> Right r
    r ->
      Left $
      "Language.JavaScript.Inline.JSON.decode: parse failed, result: " <> show r
