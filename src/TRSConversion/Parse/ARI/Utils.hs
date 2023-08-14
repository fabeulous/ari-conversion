{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

  -- * Predicates
  isNewline,
  index,

  -- * ParseErrors
  indexOutOfRangeError,
  nonPositiveNumberError,
) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Void (Void)
import Text.Megaparsec (
  MonadParsec,
  Token,
  between,
  empty,
  getOffset,
  noneOf,
  notFollowedBy,
  takeWhile1P,
  takeWhileP,
  try,
  (<?>),
 )
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as E

import TRSConversion.Parse.Utils (Parser)
import qualified TRSConversion.Problem.Common.Index as Idx

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
  _ <- (char ';' <* hspaces) <* notFollowedBy (symbol "@")
  _ <- takeWhileP Nothing (\c -> c /= '\n' && c /= '\r')
  pure ()

-- | @'spaces'@ skips any whitespace characters, or line-comment
spaces :: ARIParser ()
spaces = L.space space1 lineComment empty

-- | none or some horizontal space
hspaces :: ARIParser ()
hspaces = void $ takeWhileP Nothing (\c -> c == ' ' || c == '\t')

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
identChar :: [Char]
identChar = " \t\n\r;:()"

ident :: ARIParser String
ident =
  lexeme (unpack <$> takeWhile1P Nothing (`notElem` identChar))
    <?> "identifier"

keywordChar :: ARIParser Char
keywordChar = noneOf identChar

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

index :: ARIParser Idx.Index
index = do
  o <- getOffset
  n <- naturalNumber
  pure $ Idx.Index{Idx.index = n, Idx.startOffset = o}

isNewline :: Char -> Bool
isNewline c = c == '\n' || c == '\r'

nonPositiveNumberError :: (Token s ~ Char) => Int -> Int -> E.ParseError s e
nonPositiveNumberError n offset =
  E.TrivialError
    offset
    (Just . E.Tokens $ x :| xs)
    (Set.singleton (E.Label errStr))
 where
  (x : xs) = show n
  errStr = 'n' :| "umber greater than 0"

indexOutOfRangeError :: (Token s ~ Char) => Int -> Idx.Index -> E.ParseError s e
indexOutOfRangeError maxIndex i =
  E.TrivialError
    (Idx.startOffset i)
    (Just . E.Tokens $ n :| ns)
    (Set.singleton (E.Label errStr))
 where
  (n : ns) = show (Idx.index i)
  errStr = 'i' :| "ndex in range " ++ show (1 :: Int, maxIndex)
