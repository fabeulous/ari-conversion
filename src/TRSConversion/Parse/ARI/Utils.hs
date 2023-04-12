{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.ARI.Utils (
    module TRSConversion.Parse.Utils,
    lexeme,
    spaces,
    symbol,
    keyword,
    ident,
    sExpr,
) where

import Data.Text (Text)
import Text.Megaparsec (notFollowedBy, between, empty, noneOf, takeWhile1P, try, takeWhileP)
import Text.Megaparsec.Char (space1, string, char)
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

ident :: Parser Text
ident = takeWhile1P (Just "ident-character") (\c -> c `notElem` ("(); \t\n\r" :: [Char]))

keywordChar :: Parser Char
keywordChar = noneOf ("(); \t\n\r" :: [Char])

keyword :: Text -> Parser Text
keyword word = lexeme (try (string word <* notFollowedBy keywordChar))

sExpr :: Text -> Parser a -> Parser a
sExpr hd = between (try (symbol "(" *> keyword hd)) (symbol ")")
