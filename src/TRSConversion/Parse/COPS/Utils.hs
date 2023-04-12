{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.COPS.Utils (
    module TRSConversion.Parse.Utils,
    lexeme,
    spaces,
    symbol,
    keyword,
    ident,
    block,
) where

import Data.Text (Text)
import Text.Megaparsec (MonadParsec (notFollowedBy), between, empty, noneOf, takeWhile1P, try)
import Text.Megaparsec.Char (space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import TRSConversion.Parse.Utils (Parser)

spaces :: Parser ()
spaces = L.space space1 empty empty

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

block :: Text -> Parser a -> Parser a
block hd = between (try (string "(" *> keyword hd)) (symbol ")")
