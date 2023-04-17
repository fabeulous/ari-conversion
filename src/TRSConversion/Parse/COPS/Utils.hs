{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TRSConversion.Parse.COPS.Utils (
    module TRSConversion.Parse.Utils,
    COPSParser,
    toParser,
    lexeme,
    spaces,
    symbol,
    keyword,
    ident,
    block,
    parens,
    naturalNumber,
) where

import Data.Text (Text)
import Text.Megaparsec (between, empty, noneOf, notFollowedBy, satisfy, some, try, (<|>), MonadParsec)
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Char (isSpace)
import TRSConversion.Parse.Utils (Parser)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Void (Void)

newtype COPSParser a = COPSParser {toParser :: Parser a}
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadParsec Void Text)

spaces :: COPSParser ()
spaces = L.space space1 empty empty

lexeme :: COPSParser a -> COPSParser a
lexeme = L.lexeme spaces

symbol :: Text -> COPSParser Text
symbol = L.symbol spaces

-- | @'ident'@ parses cops identifiers
--
-- that is any string of characters not containing a whitespace, any character in
-- {(, ), ", |, \ }, and substrings "->", "==", "COMMENT", "VAR", "RULES"
ident :: COPSParser String
ident =
    lexeme
        ( try $ some
            ( (char '-' <* (notFollowedBy (char '>') <|> fail "identifiers may not contain the sequence \"->\""))
                <|> (char '=' <* (notFollowedBy (string "=") <|> fail "identifiers may not contain the sequence \"==\""))
                <|> (char 'C' <* (notFollowedBy (string "OMMENT") <|> fail "identifiers may not contain the sequence \"COMMENT\""))
                <|> (char 'V' <* (notFollowedBy (string "AR") <|> fail "identifiers may not contain the sequence \"VAR\""))
                <|> (char 'R' <* (notFollowedBy (string "ULES") <|> fail "identifiers may not contain the sequence \"RULES\""))
                <|> satisfy (\c -> not (isSpace c) && c `notElem` ("(),|\\\"" :: [Char]))
            )
        )

keywordChar :: COPSParser Char
keywordChar = noneOf ("(); \t\n\r" :: [Char])

keyword :: Text -> COPSParser Text
keyword word = lexeme (try (string word <* notFollowedBy keywordChar))

block :: Text -> COPSParser a -> COPSParser a
block hd = between (try (string "(" *> keyword hd)) (symbol ")")

parens :: COPSParser a -> COPSParser a
parens = between (symbol "(") (symbol ")")

naturalNumber :: COPSParser Int
naturalNumber = lexeme L.decimal
