{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Formats.ARI.Parse.Utils (
  -- * Type
  ARIParser,
  toParser,

  -- ** Helper types
  VarSymb,
  FunSymb,
  SortSymb,

  -- * Lexing
  lexeme,
  spaces,
  symbol,
  keyword,
  keywordToken,
  ident,
  restrictedIdent,
  naturalNumber,

  -- * Combinators
  sExpr,
  sExpr',
  sExpr'',
  parens,
  noSExpr,

  -- * Predicates
  isNewline,
  index,

  -- * ParseErrors
  indexOutOfRangeError,
  nonPositiveNumberError,
  duplicateIndex,
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
  label,
  noneOf,
  notFollowedBy,
  showErrorComponent,
  takeWhile1P,
  takeWhileP,
  try,
  many,
  (<?>),
  (<|>),
 )
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Error as E

import TRSConversion.Parse.Utils (Parser, Token (..), mkToken)
import qualified TRSConversion.Problem.Common.Index as Idx
import qualified Text.Megaparsec.Error.Builder as E

type FunSymb = Token String
type VarSymb = Token String
type SortSymb = Token String

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
  | DuplicateIndex Int
  deriving (Eq, Ord)

instance Show ARIParseError where
  show = showErrorComponent

isInvalidIdentifier :: String -> String -> ARIParseError
isInvalidIdentifier = InvalidIdentifier

mustBeBelow :: Int -> Int -> ARIParseError
mustBeBelow = IndexOutOfRange

mustBeUnique :: Int -> ARIParseError
mustBeUnique = DuplicateIndex

instance ShowErrorComponent ARIParseError where
  showErrorComponent (DuplicateIndex n) =
    "index " ++ show n ++ " must appear at most once"
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
  errorComponentLen (DuplicateIndex n) = length (show n)

customErr :: ARIParseError -> E.EF ARIParseError
customErr = E.fancy . ErrorCustom

{- | suceeds if no parentheses is opened. Use when no more sExpression is expected, like before @eof@.
 this lead to better error messages when an sExpression is upened unexpectedly.

Parsing "( unknownName )" leads to unexpected "unknownName",
instead of unexpected '('
-}
noSExpr :: ARIParser ()
noSExpr =
  ( parens
      $ do
        o <- getOffset
        n <- ident
        parseError $ E.err o $ E.utoks (tokenText n)
  )
    <|> pure ()

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

ident :: ARIParser (Token String)
ident = lexeme $ do
  txt <- tokenOfText (takeWhile1P Nothing (`notElem` identChar)) <?> "identifier"
  pure $ fmap unpack txt

ident'Char :: [Char]
ident'Char = " \t\n\r;()"

ident' :: ARIParser (Token String)
ident' = lexeme $ do
  txt <- tokenOfText (takeWhile1P Nothing (`notElem` ident'Char)) <?> "identifier"
  pure $ fmap unpack txt


keywords :: [String]
keywords =
  ["format", "fun", "sort", "rule", "theory", "define-fun"]
    -- ++ ["oriented", "join", "semi-equational"]
    ++ ["TRS", "MSTRS", "LCSTRS", "CTRS", "CSTRS", "CSCTRS"]

restrictedIdent :: ARIParser (Token String)
restrictedIdent = label "identifier" . lexeme $ do
  o <- getOffset
  identifier' <- tokenOfText $ takeWhile1P Nothing (`notElem` identChar)
  let identifier = unpack <$> identifier'
  when (tokenValue identifier `elem` keywords)
    $ parseError
    $ E.errFancy o (customErr (unpack (tokenText identifier) `isInvalidIdentifier` "because it is a reserved keyword"))
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

keywordToken :: Text -> ARIParser (Token Text)
keywordToken word = lexeme . tokenOfText $ try (string word <* notFollowedBy keywordChar)

-- | @'sExpr' name p@ parses as s-expression with the head @name@ and content @p@.
sExpr :: Text -> ARIParser a -> ARIParser a
sExpr hd = between (try (symbol "(" *> keyword hd)) (symbol ")")

-- | @'sExpr\'' name p@ parses as s-expression, where the head is parsed by @name@ and content @p@.
sExpr' :: ARIParser b -> ARIParser a -> ARIParser a
sExpr' p = between (try (symbol "(" *> p)) (symbol ")")

data LTree l
  = Leaf l
  | Node [LTree l]

sExpr'' :: ARIParser (LTree (Token String))
sExpr'' = ( symbol "(" *> many sExpr'' >>= \es -> symbol ")" *> return (Node es) ) <|> ( ident' >>= \i -> return (Leaf i) )

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
  E.errFancy (Idx.startOffset i)
    $ customErr (Idx.index i `mustBeBelow` maxIndex)

duplicateIndex :: Idx.Index -> E.ParseError s ARIParseError
duplicateIndex i =
  E.errFancy (Idx.startOffset i) $ customErr (mustBeUnique (Idx.index i))

tokenOfText :: ARIParser Text -> ARIParser (Token Text)
tokenOfText p = do
  start <- getOffset
  value <- p
  end <- getOffset
  pure $ mkToken value start (end - start) value
