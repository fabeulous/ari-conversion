{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.COPS.Utils (
    -- * Type
    COPSParser,
    toParser,

    -- * Lexing
    lexeme,
    spaces,
    symbol,
    keyword,
    ident,
    naturalNumber,

    -- * Combinators
    block,
    parens,
) where

import Data.Text (Text, pack)
import Text.Megaparsec (
    MonadParsec,
    between,
    choice,
    empty,
    noneOf,
    notFollowedBy,
    satisfy,
    some,
    try,
    (<|>),
 )
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Char (isSpace)
import Data.Void (Void)
import TRSConversion.Parse.Utils (Parser)

-- | Type of the COPS parser.
newtype COPSParser a = COPSParser {toParser :: Parser a}
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadPlus
        , MonadFail
        , MonadParsec Void Text
        )

-- | @'spaces'@ skips any whitespace characters
spaces :: COPSParser ()
spaces = L.space space1 empty empty

-- | @'lexeme' p@ skips trailing whitespace after @p@
lexeme :: COPSParser a -> COPSParser a
lexeme = L.lexeme spaces

-- | @'symbol' name@ parses a symbol @name@ and skips trailing whitespace
symbol :: Text -> COPSParser Text
symbol = L.symbol spaces

{- | @'ident'@ parses cops identifiers

that is any string of characters not containing a whitespace, any character in
{(, ), ", |, \ }, and sub-strings "->", "==", "COMMENT", "VAR", "RULES"
-}
ident :: COPSParser String
ident = lexeme (try . some $ identChar)
  where
    failSubseq s =
        fail ("identifiers may not contain the sequence \"" ++ s ++ "\"")

    -- parses a character as long as it is not part of the sequence
    charNotSubSeq :: String -> COPSParser Char
    charNotSubSeq s@(x : xs) =
        char x <* (notFollowedBy (string (pack xs)) <|> failSubseq s)
    charNotSubSeq [] = empty

    identChar =
        choice
            [ charNotSubSeq "->"
            , charNotSubSeq "=="
            , charNotSubSeq "COMMENT"
            , charNotSubSeq "VAR"
            , charNotSubSeq "RULES"
            , satisfy
                ( \c ->
                    not (isSpace c)
                        && c `notElem` ("(),|\\\"" :: [Char])
                )
            ]

keywordChar :: COPSParser Char
keywordChar = noneOf ("(); \t\n\r" :: [Char])

{- | @'keyword' name@ defines and parses a keyword.

The parser only succeeds if the keyword is followed by a character in
@"(); \t\n\r"@ or a @eof@. This makes sure that we parse full keywords,
not only a prefix.
-}
keyword :: Text -> COPSParser Text
keyword word = lexeme (try (string word <* notFollowedBy keywordChar))

{- | @'block' p@ parses a COPS block with the header @name@ and content @p@.

A COPS block starts with a opening parenthesis, followed by the @name@,
it ends with a closing parenthesis. (e.g, "(VAR x y z)" where "x y z" is
consumed by p)
-}
block :: Text -> COPSParser a -> COPSParser a
block hd = between (try (string "(" *> keyword hd)) (symbol ")")

{- | @'parens' p@ parses @'('@ followed by @p@ followed by @')'@.

It also consumes trailing whitespace after the parentheses (but not after @p@).
-}
parens :: COPSParser a -> COPSParser a
parens = between (symbol "(") (symbol ")")

{- | @'naturalNumber'@ parses a natural number.

It consumes trailing whitespace
-}
naturalNumber :: COPSParser Int
naturalNumber = lexeme L.decimal
