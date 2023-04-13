{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.ARI.Utils (
  module TRSConversion.Parse.Utils,
  lexeme,
  spaces,
  symbol,
  keyword,
  ident,
  sExpr,
  parens,
) where

import Data.Text (Text, unpack)
import Text.Megaparsec (between, empty, noneOf, notFollowedBy, takeWhile1P, takeWhileP, try, (<?>))
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import TRSConversion.Parse.Utils (Parser)

lineComment :: Parser ()
lineComment = do
  _ <- char ';' <* notFollowedBy " @"
  _ <- takeWhileP Nothing (/= '\n')
  pure ()

spaces :: Parser ()
spaces = L.space space1 lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser Text
symbol = L.symbol spaces

ident :: Parser String
ident = lexeme (unpack <$> takeWhile1P Nothing (\c -> c `notElem` ("(); \t\n\r" :: [Char])))
        <?> "identifier"

keywordChar :: Parser Char
keywordChar = noneOf ("(); \t\n\r" :: [Char])

keyword :: Text -> Parser Text
keyword word = lexeme (try (string word <* notFollowedBy keywordChar))

sExpr :: Text -> Parser a -> Parser a
sExpr hd = between (try (symbol "(" *> keyword hd)) (symbol ")")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
