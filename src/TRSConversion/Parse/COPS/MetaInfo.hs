{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : TRSConversion.Parse.COPS.MetaInfo
Description : Comment parser

This module defines parsers to parse the additional information (comment, author, etc.) of a
given rewriting system.
-}
module TRSConversion.Parse.COPS.MetaInfo (
  -- * COPS
  parseCopsMetaInfo,
  parseCopsMetaInfoBlock,
)
where

import Data.Char (isSpace)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (unpack)
import Text.Megaparsec (
  MonadParsec (lookAhead, notFollowedBy),
  between,
  choice,
  getOffset,
  hidden,
  noneOf,
  option,
  optional,
  region,
  sepBy1,
  sepEndBy,
  setOffset,
  some,
  takeP,
  takeWhile1P,
  takeWhileP,
  try,
  unexpected,
  (<?>),
  (<|>),
 )
import Text.Megaparsec.Char (char, hspace, space, string)
import qualified Text.Megaparsec.Error as E

import TRSConversion.Parse.COPS.Utils (COPSParser, block)
import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo, mergeMetaInfo)

parseCopsMetaInfoBlock :: COPSParser MetaInfo
parseCopsMetaInfoBlock = block "COMMENT" parseCopsMetaInfo

{- | Parser to extract comments as a @String@ from the (optional) @COMMENT@
block of the COPS TRS format.
-}
parseCopsMetaInfo :: COPSParser MetaInfo
parseCopsMetaInfo = do
  ls <- sepEndBy (metaDoi <|> metaAuthors <|> metaComment) space
  return $ foldr mergeMetaInfo emptyMetaInfo ls
 where
  metaDoi = (\v -> emptyMetaInfo{doi = Just v}) <$> copsDoiLine
  metaAuthors = (\v -> emptyMetaInfo{submitted = Just v}) <$> copsAuthorsLine
  metaComment = (\v -> emptyMetaInfo{comment = Just [v]}) <$> copsCommentLine

copsDoiLine :: COPSParser String
copsDoiLine = string "doi:" *> (pDoi <?> "doi")
 where
  pDoi = unpack <$> takeWhileP Nothing (not . isSpace)

copsAuthorsLine :: COPSParser [String]
copsAuthorsLine = string "submitted by:" *> hspace *> (pAuthors <?> "authors")
 where
  pAuthors =
    sepBy1
      (name <?> "name")
      ( choice
          [ string "," <* hspace <* optional (word "and")
          , word "and"
          ]
      )

  nonWordChars = [',', ';', '(', ')', ' ', '\t', '\n', '\r']

  word str = string str <* notFollowedBy (noneOf nonWordChars) <* hidden hspace

  name = fmap unwords . some . try $ do
    o <- getOffset
    nm <- takeWhile1P Nothing (`notElem` nonWordChars)
    if nm == "and"
      then region (E.setErrorOffset o) (unexpected (E.Tokens ('a' :| "nd")))
      else unpack nm <$ hidden hspace

copsCommentLine :: COPSParser String
copsCommentLine = do
  pref <- unpack <$> takeWhile1P Nothing (`notElem` ['\n', '\r', '(', ')'])
  rest <- option "" $ do
    par <- between (char '(') (char ')') parseComment
    suf <- option "" copsCommentLine
    pure $ '(' : par ++ ')' : suf
  pure $ pref ++ rest

{- | Parser to parse a string comment between two outermost parentheses.
Calls itself recursively to allow for nested parentheses inside comments.

The recursive logic is adapted from the library @term-rewriting@.
-}
parseComment :: COPSParser String
parseComment =
  withParens
    <|> (++) <$> some (noneOf ['(', ')']) <*> parseComment
    <|> return ""
 where
  withParens :: COPSParser String
  withParens = do
    _ <- char '('
    pre <- parseComment
    _ <- char ')'
    suf <- parseComment
    return $ "(" ++ pre ++ ")" ++ suf
