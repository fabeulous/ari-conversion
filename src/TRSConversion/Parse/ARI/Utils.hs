{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module TRSConversion.Parse.ARI.Utils (
  module TRSConversion.Parse.Utils,
  ARIParser,
  toParser,
  lexeme,
  spaces,
  symbol,
  keyword,
  ident,
  sExpr,
  parens,
  naturalNumber,
) where

import Data.Text (Text, unpack)
import Text.Megaparsec (MonadParsec, between, empty, noneOf, notFollowedBy, takeWhile1P, takeWhileP, try, (<?>))
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Void (Void)
import TRSConversion.Parse.Utils (Parser)

newtype ARIParser a = ARIParser {toParser :: Parser a}
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadFail, MonadParsec Void Text)

lineComment :: ARIParser ()
lineComment = do
  _ <- char ';' <* notFollowedBy (string " @")
  _ <- takeWhileP Nothing (/= '\n')
  pure ()

spaces :: ARIParser ()
spaces = L.space space1 lineComment empty

lexeme :: ARIParser a -> ARIParser a
lexeme = L.lexeme spaces

symbol :: Text -> ARIParser Text
symbol = L.symbol spaces

ident :: ARIParser String
ident =
  lexeme (unpack <$> takeWhile1P Nothing (\c -> c `notElem` ("(); \t\n\r" :: [Char])))
    <?> "identifier"

keywordChar :: ARIParser Char
keywordChar = noneOf ("(); \t\n\r" :: [Char])

keyword :: Text -> ARIParser Text
keyword word = lexeme (try (string word <* notFollowedBy keywordChar))

sExpr :: Text -> ARIParser a -> ARIParser a
sExpr hd = between (try (symbol "(" *> keyword hd)) (symbol ")")

parens :: ARIParser a -> ARIParser a
parens = between (symbol "(") (symbol ")")

naturalNumber :: ARIParser Int
naturalNumber = lexeme L.decimal
