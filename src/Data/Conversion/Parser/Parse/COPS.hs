{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Conversion.Parser.Parse.COPS
  (
  )
where

import Control.Monad.Error
import Data.Conversion.Problem.Rule (Rule (..), parseRule)
import qualified Data.Conversion.Problem.Rule as Rule
import Data.Conversion.Problem.TRS (TRS (..))
import qualified Data.Conversion.Problem.TRS as TRS
import Data.Conversion.Problem.Term (parseTerm, parseVariable)
import Data.Conversion.Utils (lex, lexeme, par, sc)
import Data.Rewriting.Term.Type
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    anySingle,
    between,
    eof,
    many,
    noneOf,
    optional,
    parseTest,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char
  ( char,
    spaceChar,
    string,
  )
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (ErrorFancy (..))
import Prelude hiding (lex)

type Parser = Parsec Void Text

type Vars = [String]

-- | Parser to extract the 'Rule's from a RULES block of the COPS TRS format
--   Expects format @lhs -> rhs@ and calls the 'Term' parser 'parseTerm' on each side of the rule
-- qqjf succeeds on no rules
parseRules :: Vars -> Parser [Rule String String]
parseRules vs = many (parseRule vs)

-- | Strip spaces at start and end of a string
stripSpaces :: Parser a -> Parser a
stripSpaces p = lexeme (many spaceChar *> p)

-- | Parse a problem in COPS format
-- qqjf copy in COPS grammar
parseCOPS :: Parser (TRS String String)
parseCOPS = stripSpaces $ do
  vs <- try $ block "VAR" (many parseVariable)
  rules <- block "RULES" (parseRules vs)
  comment <- optional (block "COMMENT" parseComment)
  return
    ( TRS
        { rules = rules,
          variables = vs,
          signature = [], -- [(f, Int)]
          comment = comment
        }
    )
  where
    block :: String -> Parser a -> Parser a
    block name p = parens (parseBlockName *> lexeme p) <?> (name ++ " block")
      where
        parseBlockName = lexeme $ string (pack name)

-- | Parser to extract comments as a @String@ from a @COMMENT@ block of the COPS TRS format
-- Runs recursively to allow for parentheses inside comments qqjf
-- Copied from library term-rewriting
parseComment :: Parser String
parseComment =
  withParens
    <|> (++) <$> some (noneOf ['(', ')']) <*> parseComment
    <|> return ""
  where
    withParens :: Parser String
    withParens = do
      _ <- char '('
      pre <- parseComment
      _ <- char ')'
      suf <- parseComment
      return $ "(" ++ pre ++ ")" ++ suf

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Strip outer parentheses
parens :: Parser a -> Parser a
parens = between (lexeme $ symbol "(") (lexeme $ symbol ")")
