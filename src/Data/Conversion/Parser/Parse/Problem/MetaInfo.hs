{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parser.Parse.Problem.MetaInfo
-- Description : Comment parser
--
-- This module defines parsers to parse the additional information (comments, author, etc.) of a given TRS.
module Data.Conversion.Parser.Parse.Problem.MetaInfo
  ( parseCopsMetaInfo,
    parseAriMetaInfo,
  )
where

import Data.Conversion.Parser.Parse.Utils (Parser, parseBlock)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Text.Megaparsec
  ( between,
    noneOf,
    optional,
    sepBy,
    some,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (char, spaceChar)

-- | Parser to extract comments as a @String@ from a @COMMENT@ block of the COPS TRS format.
-- qqjf should be updated to infer structure from comments (e.g. author, source, etc.).
parseCopsMetaInfo :: Parser MetaInfo
parseCopsMetaInfo = do
  comment <- parseComment
  return $ emptyMetaInfo {comments = Just [comment]}

-- Parser to parse a string comment between two outermost parentheses.
-- Calls itself recursively to allow for nested parentheses inside comments.
--
-- The recursive logic is adapted from the library @term-rewriting@.
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

-- | Parse meta-info blocks in a TRS in ARI format.
-- Recursively updates the current meta-info state.
--
-- Expects e.g. something like
-- > (meta-info (origin "COPS #20"))
-- > (meta-info (doi "10.1007/11805618_6"))
-- > (meta-info (comment "[7] Example 2"))
-- > (meta-info (submitted "Takahito Aoto" "Junichi Yoshida" "Yoshihito Toyama"))
parseAriMetaInfo :: Parser MetaInfo
parseAriMetaInfo = go emptyMetaInfo
  where
    -- Recursively update MetaInfo from top to bottom
    go :: MetaInfo -> Parser MetaInfo
    go oldMeta = do
      maybeMeta <- optional $ try (parseBlock "meta-info" (parseAriMetaInfoBlock oldMeta))
      case maybeMeta of
        Nothing -> return oldMeta
        Just updatedMeta -> go updatedMeta

-- | Parse a single meta-info block for a TRS in ARI format and updates the given 'MetaInfo' variable.
-- The parsers for each block type are deliberately left flexible
-- to allow adding additional validation checks or constraints later.
--
-- qqjf Currently overwrites duplicate doi, origin, and submitted values. I was unsure what the desired behaviour is here.
parseAriMetaInfoBlock :: MetaInfo -> Parser MetaInfo
parseAriMetaInfoBlock meta@(MetaInfo oldComments _ _ _) = try parseAriComment <|> try parseDoi <|> try parseOrigin <|> try parseSubmitters <?> "meta-info"
  where
    parseAriComment :: Parser MetaInfo
    parseAriComment = do
      comment <- parseBlock "comment" parseComment
      let newComments = case oldComments of
            Nothing -> Just [comment]
            Just cs -> Just $ cs ++ [comment] -- Append new comment to comments list qqjf
      return $ meta {comments = newComments}
    parseDoi :: Parser MetaInfo
    parseDoi = do
      newDoi <- parseBlock "doi" parseComment
      return $ meta {doi = Just newDoi} -- Overwrite old doi qqjf
    parseOrigin :: Parser MetaInfo
    parseOrigin = do
      newOrigin <- parseBlock "origin" parseComment
      return $ meta {origin = Just newOrigin} -- Overwrite old origin qqjf
    parseSubmitters :: Parser MetaInfo
    parseSubmitters = do
      submitters <- parseBlock "submitted" (parseSubmitterName `sepBy` (some spaceChar))
      return $ meta {submitted = Just submitters}
    -- Parse a name wrapped in @"@s
    parseSubmitterName = between (char '"') (char '"') (some $ noneOf ['(', ')', '"'])