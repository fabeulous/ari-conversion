{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Parser.Parse.Utils
  ( lexeme,
    Parser,
    sc,
    parens,
    symbol,
    stripSpaces,
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, between, many)
import Text.Megaparsec.Char (space1, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L

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

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Strip outer parentheses
parens :: Parser a -> Parser a
parens = between (lexeme $ symbol "(") (lexeme $ symbol ")")

-- | Strip spaces at start and end of a string
stripSpaces :: Parser a -> Parser a
stripSpaces p = lexeme (many spaceChar *> p)