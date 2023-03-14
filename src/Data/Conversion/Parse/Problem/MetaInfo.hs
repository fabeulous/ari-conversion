{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parse.Problem.MetaInfo
-- Description : Comment parser
--
-- This module defines parsers to parse the additional information (comment, author, etc.) of a
-- given rewriting system.
module Data.Conversion.Parse.Problem.MetaInfo
  ( -- * COPS
    parseCopsMetaInfo,

    -- * ARI
    parseAriMetaInfo,
  )
where

import Data.Conversion.Parse.Utils (Parser, lexeme, parseBlock)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo)
import Text.Megaparsec (between, many, noneOf, optional, sepBy, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (char, spaceChar)

-- | Parser to extract comments as a @String@ from the (optional) @COMMENT@ block of the COPS TRS format.
--
-- qqjf should this be updated to try to infer structure from comments (e.g. author, source, etc.)?
parseCopsMetaInfo :: Parser MetaInfo
parseCopsMetaInfo = do
  cs <- parseComment
  return $ emptyMetaInfo {comment = Just cs}

-- | Parser to parse a string comment between two outermost parentheses.
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

-- | Parse meta-info blocks in a TRS in ARI format by starting with 'emptyMetaInfo' and
-- recursively updating the current 'MetaInfo' value.
--
-- Expects e.g. something like
-- @
-- (meta-info (origin "COPS #20"))
-- (meta-info (doi "10.1007/11805618_6"))
-- (meta-info (comment "[7] Example 2"))
-- (meta-info (submitted "Takahito Aoto" "Junichi Yoshida" "Yoshihito Toyama"))
-- @.
--
-- See the tests for more examples of expected inputs.
--
-- qqjf I wasn't sure what the behaviour should be if multiple blocks are specified, so this
-- implementation currently overwrites duplicate doi, origin, and submitted values.
parseAriMetaInfo :: Parser MetaInfo
parseAriMetaInfo = go emptyMetaInfo
  where
    -- Recursively update MetaInfo from start to end of stream
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
-- qqjf Currently overwrites duplicate doi, origin, and submitted values.
parseAriMetaInfoBlock :: MetaInfo -> Parser MetaInfo
parseAriMetaInfoBlock meta =
  lexeme
    ( try parseAriComment
        <|> try parseDoi
        <|> try parseOrigin
        <|> try parseSubmitters
    )
    <?> "meta-info"
  where
    parseAriComment, parseDoi, parseOrigin, parseSubmitters :: Parser MetaInfo
    parseAriComment = do
      newComment <- parseCommentBlock "comment"
      return $ meta {comment = Just newComment} -- Overwrite old comment
    parseDoi = do
      newDoi <- parseCommentBlock "doi"
      return $ meta {doi = Just newDoi} -- Overwrite old doi
    parseOrigin = do
      newOrigin <- parseCommentBlock "origin"
      return $ meta {origin = Just newOrigin} -- Overwrite old origin
    parseSubmitters = do
      submitters <- lexeme $ parseBlock "submitted" (parseSubmitterName `sepBy` some spaceChar)
      return $ meta {submitted = Just submitters}
    -- Parse a non-empty name wrapped in @"@s
    parseSubmitterName = between (char '"') (char '"') (some $ noneOf ['(', ')', '"'])
    -- Parse a (possible empty) comment between @"@s
    parseCommentBlock name = parseBlock name (between (char '"') (char '"') (many $ noneOf ['"']))