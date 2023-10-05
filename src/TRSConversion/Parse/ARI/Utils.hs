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
  restrictedIdent,
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
import Control.Monad (MonadPlus, when)
import Data.Functor (void)
import Data.Text (Text, unpack)
import Text.Megaparsec (
  ErrorFancy (..),
  MonadParsec (parseError),
  ShowErrorComponent,
  between,
  empty,
  getOffset,
  noneOf,
  notFollowedBy,
  takeWhile1P,
  takeWhileP,
  try,
  (<?>), showErrorComponent,
 )
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as E

import TRSConversion.Parse.Utils (Parser)
import qualified TRSConversion.Problem.Common.Index as Idx
import qualified Text.Megaparsec.Error.Builder as E

-- | Type of the COPS parser.
newtype ARIParser a = ARIParser {toParser :: Parser ARIParseError a}
  deriving
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadPlus
    , MonadFail
    , MonadParsec ARIParseError Text
    )

data ARIParseError
  = InvalidIdentifier String String
  | IndexOutOfRange Int Int
  | NonPositiveNumber Int
  deriving (Eq, Ord)


instance Show ARIParseError where
  show = showErrorComponent

isInvalidIdentifier :: String -> String -> ARIParseError
isInvalidIdentifier = InvalidIdentifier

mustBeBelow :: Int -> Int -> ARIParseError
mustBeBelow = IndexOutOfRange

instance ShowErrorComponent ARIParseError where
  showErrorComponent (NonPositiveNumber n) =
     "number " ++ show n ++ " must be greater than 0"
  showErrorComponent (IndexOutOfRange n maxInd) =
    "index " ++ show n ++ " out of range, it must be at most " ++ show maxInd
  showErrorComponent (InvalidIdentifier name reason) =
    "'"
      ++ name
      ++ "' is not a valid identifier"
      ++ if null reason then "" else ",\n" ++ reason

  errorComponentLen (InvalidIdentifier name _) = length name
  errorComponentLen (IndexOutOfRange n _) = length (show n)
  errorComponentLen (NonPositiveNumber n) = length (show n)

customErr :: ARIParseError -> E.EF ARIParseError
customErr = E.fancy . ErrorCustom

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

keywords :: [String]
keywords =
  ["format", "fun", "sort", "rule", "theory", "define-fun"]
    -- ++ ["oriented", "join", "semi-equational"]
    ++ ["TRS", "MSTRS", "LCSTRS", "CTRS", "CSTRS", "CSCTRS"]

restrictedIdent :: ARIParser String
restrictedIdent = lexeme $ do
  o <- getOffset
  identifier <- unpack <$> takeWhile1P Nothing (`notElem` identChar)
  when (identifier `elem` keywords) $
    parseError $
      E.errFancy o (customErr $ identifier `isInvalidIdentifier` "because it is a reserved keyword")
  pure identifier

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

nonPositiveNumberError :: Int -> Int -> E.ParseError s ARIParseError
nonPositiveNumberError n offset =
  E.errFancy offset $ customErr (NonPositiveNumber n)

indexOutOfRangeError :: Int -> Idx.Index -> E.ParseError s ARIParseError
indexOutOfRangeError maxIndex i =
  E.errFancy (Idx.startOffset i) $
    customErr (Idx.index i `mustBeBelow` maxIndex)
