{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Utils
  ( lex,
    par,
    ident,
    lexeme,
    Parser,
    sc,
  )
where

import Control.Monad
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
  )
import Text.Megaparsec.Char
  ( space1,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Parsec (ParsecT, Stream, between, char, many1, satisfy, spaces, try)
import Prelude hiding (lex)

-- | @lex p@ is similar to @p@ but also consumes trailing white space.
-- Copied from https://github.com/haskell-rewriting/term-rewriting/blob/master/src/Data/Rewriting/Utils/Parse.hs
lex :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lex p = do x <- p; spaces; return x

-- | @par p@ accpets @p@ enclosed in parentheses ('@(@' and '@)@').
-- Copied from https://github.com/haskell-rewriting/term-rewriting/blob/master/src/Data/Rewriting/Utils/Parse.hs
par :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
par = between (lex $ char '(') (lex $ char ')')

-- | @ident taboo@ parses a non-empty sequence of non-space characters not
-- containing elements of @taboo@.
-- Copied from https://github.com/haskell-rewriting/term-rewriting/blob/master/src/Data/Rewriting/Utils/Parse.hs
ident :: Stream s m Char => String -> [String] -> ParsecT s u m String
ident tabooChars tabooWords = try $ do
  s <- many1 (satisfy (\c -> not (isSpace c) && c `notElem` tabooChars))
  guard (s `notElem` tabooWords)
  return s

-- | Type alias qqjf
type Parser = Parsec Void Text

-- | Space handler qqjf
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | lexeme is a lexeme that consumes all trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
