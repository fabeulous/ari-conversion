{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.COPS.Utils (
    module TRSConversion.Parse.Utils,
    lexeme,
    spaces,
    symbol,
    keyword,
    ident,
    block,
    parens,
) where

import Data.Text (Text)
import Text.Megaparsec (between, empty, noneOf, notFollowedBy, satisfy, some, try, (<|>))
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (isSpace)
import TRSConversion.Parse.Utils (Parser)

spaces :: Parser ()
spaces = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaces

symbol :: Text -> Parser Text
symbol = L.symbol spaces

-- | @'ident'@ parses cops identifiers
--
-- that is any string of characters not containing a whitespace, any character in
-- {(, ), ", |, \ }, and substrings "->", "==", "COMMENT", "VAR", "RULES"
ident :: Parser String
ident =
    lexeme
        ( some
            ( (char '-' <* (notFollowedBy (char '>') <|> fail "identifiers may not contain the sequence \"->\""))
                <|> (char '=' <* (notFollowedBy (string "=") <|> fail "identifiers may not contain the sequence \"==\""))
                <|> (char 'C' <* (notFollowedBy (string "OMMENT") <|> fail "identifiers may not contain the sequence \"COMMENT\""))
                <|> (char 'V' <* (notFollowedBy (string "AR") <|> fail "identifiers may not contain the sequence \"VAR\""))
                <|> (char 'R' <* (notFollowedBy (string "ULES") <|> fail "identifiers may not contain the sequence \"RULES\""))
                <|> satisfy (\c -> not (isSpace c) && c `notElem` ("(),|\\\"" :: [Char]))
            )
        )

keywordChar :: Parser Char
keywordChar = noneOf ("(); \t\n\r" :: [Char])

keyword :: Text -> Parser Text
keyword word = lexeme (try (string word <* notFollowedBy keywordChar))

block :: Text -> Parser a -> Parser a
block hd = between (try (string "(" *> keyword hd)) (symbol ")")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
