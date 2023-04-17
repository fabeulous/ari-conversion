{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.ARI.Utils (
  -- * Type
  ARIParser,
  toParser,

  -- * Lexing
  lexeme,
  spaces,
  symbol,
  keyword,
  ident,
  naturalNumber,

  -- * Combinators
  sExpr,
  parens,
) where

import Data.Text (Text, unpack)
import Text.Megaparsec (
  MonadParsec,
  between,
  empty,
  noneOf,
  notFollowedBy,
  takeWhile1P,
  takeWhileP,
  try,
  (<?>),
 )
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Void (Void)
import TRSConversion.Parse.Utils (Parser)

-- | Type of the COPS parser.
newtype ARIParser a = ARIParser {toParser :: Parser a}
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadFail
    , MonadParsec Void Text
    )

{- | @'lineComment'@ consumes a line-comment, which starts with a @;@ character
and continues until a newline character (the newline character is not consumed).
-}
lineComment :: ARIParser ()
lineComment = do
  _ <- char ';' <* notFollowedBy (string " @")
  _ <- takeWhileP Nothing (\c -> c /= '\n' && c /= '\r')
  pure ()

-- | @'spaces'@ skips any whitespace characters, or line-comment
spaces :: ARIParser ()
spaces = L.space space1 lineComment empty

-- | @'lexeme' p@ skips trailing whitespace after @p@
lexeme :: ARIParser a -> ARIParser a
lexeme = L.lexeme spaces

-- | @'symbol' name@ parses a symbol @name@ and skips trailing whitespace
symbol :: Text -> ARIParser Text
symbol = L.symbol spaces

{- | @'ident'@ parses cops identifiers

that is any string of characters not containing a whitespace, any character in
{(, ), ;}
-}
ident :: ARIParser String
ident =
  lexeme (unpack <$> takeWhile1P Nothing (`notElem` ("(); \t\n\r" :: [Char])))
    <?> "identifier"

keywordChar :: ARIParser Char
keywordChar = noneOf ("(); \t\n\r" :: [Char])

{- | @'keyword' name@ defines and parses a keyword.

The parser only succeeds if the keyword is followed by a character in
@"(); \t\n\r"@ or a @eof@. This makes sure that we parse full keywords,
not only a prefix.
-}
keyword :: Text -> ARIParser Text
keyword word = lexeme (try (string word <* notFollowedBy keywordChar))

-- | @'block' p@ parses as s-expression with the head @name@ and content @p@.
sExpr :: Text -> ARIParser a -> ARIParser a
sExpr hd = between (try (symbol "(" *> keyword hd)) (symbol ")")

{- | @'parens' p@ parses @'('@ followed by @p@ followed by @')'@.

It also consumes trailing whitespace after the parentheses (but not after @p@).
-}
parens :: ARIParser a -> ARIParser a
parens = between (symbol "(") (symbol ")")

{- | @'naturalNumber'@ parses a natural number.

It consumes trailing whitespace
-}
naturalNumber :: ARIParser Int
naturalNumber = lexeme L.decimal
