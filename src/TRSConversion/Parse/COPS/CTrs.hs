{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Conversion.Parse.ParseTrs
Description : Parser for first-order TRSs

This module defines functions to parse a first-order TRS in COPS and ARI format.
-}
module Data.Conversion.Parse.ParseCTrs (
  -- ** COPS
  parseCopsCTrs,
)
where

import Data.Char (isAlphaNum, isSpace)
import Data.Conversion.Problem.CTrs.CTrs (CRule (..), Condition (..))
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Functor (($>))
import Data.Text (Text, unpack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (notFollowedBy), Parsec, between, empty, many, option, satisfy, sepBy, sepBy1, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Conversion.Problem.Common.MetaInfo (emptyMetaInfo, MetaInfo)
import Data.Conversion.Parse.Problem.MetaInfo (parseCopsMetaInfo)

type Parser = Parsec Void Text

parseCopsCTrs :: Parser ParsedCTRS
parseCopsCTrs = do
  condType <- pCondTypeBlock
  vars <- option [] pVarsBlock
  rs <- pRulesBlock vars
  metaInf <- option emptyMetaInfo (block "COMMENT" parseCopsMetaInfo)
  return $
    CTRS
      { conditionType = condType
      , variables = vars
      , rules = rs
      , metaInfo = metaInf
      }

data CondType = Oriented | Join | SemiEquational
  deriving (Show)

-- Utils movoe
data ParsedCTRS = CTRS
  { conditionType :: CondType
  , variables :: [String]
  , rules :: [CRule String String]
  , metaInfo :: MetaInfo
  }
  deriving (Show)

space :: Parser ()
space = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol symb = lexeme (string symb)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pIdent :: Parser String
pIdent =
  lexeme
    ( some
        ( (char '-' <* (notFollowedBy (char '>') <|> fail "identifiers may not contain the sequence \"->\""))
            <|> (char '=' <* (notFollowedBy (string "=") <|> fail "identifiers may not contain the sequence \"==\""))
            <|> (char 'C' <* (notFollowedBy (string "OMMENT") <|> fail "identifiers may not contain the sequence \"COMMENT\""))
            <|> (char 'V' <* (notFollowedBy (string "AR") <|> fail "identifiers may not contain the sequence \"VAR\""))
            <|> (char 'R' <* (notFollowedBy (string "ULES") <|> fail "identifiers may not contain the sequence \"RULES\""))
            <|> satisfy (\c -> not (isSpace c) && c `notElem` ("(),|\\\"" :: [Char]))
        )
        <?> "identifier"
    )

block :: Text -> Parser a -> Parser a
block hd pBody =
  try (symbol "(" *> pKeyword hd) *> pBody <* symbol ")"

keywordCharset :: Parser Char
keywordCharset = satisfy (\c -> isAlphaNum c || c `elem` ['-', '_'])

pKeyword :: Text -> Parser Text
pKeyword name = lexeme (try (string name <* notFollowedBy keywordCharset)) <?> unpack name

pCondType :: Parser CondType
pCondType =
  (pKeyword "ORIENTED" $> Oriented)
    <|> (pKeyword "JOIN" $> Join)
    <|> (pKeyword "SEMI-EQUATIONAL" $> SemiEquational)

pCondTypeBlock :: Parser CondType
pCondTypeBlock = block "CONDITIONTYPE" pCondType <?> "CONDITIONTYPE block"

pVarsBlock :: Parser [String]
pVarsBlock = block "VAR" pVars <?> "VAR block"
 where
  pVars = many pIdent

pRulesBlock :: [String] -> Parser [CRule String String]
pRulesBlock vars = block "RULES" pRules <?> "RULES Block"
 where
  pRules = many (pRule vars)

pRule :: [String] -> Parser (CRule String String)
pRule vars = CRule <$> pTerm vars <*> (symbol "->" *> pTerm vars) <*> pConditions
 where
  pConditions =
    option [] $ symbol "|" *> sepBy1 pCondition (symbol ",")

  pCondition = (:==) <$> pTerm vars <*> (symbol "==" *> pTerm vars) <?> "condition"

pTerm :: [String] -> Parser (Term String String)
pTerm vars = do
  name <- pIdent
  if name `elem` vars
    then pure $ Var name
    else Fun name <$> option [] (parens (sepBy (pTerm vars) (symbol ",")))
