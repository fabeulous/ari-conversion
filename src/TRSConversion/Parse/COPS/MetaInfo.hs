{-# LANGUAGE OverloadedStrings, TypeFamilies #-}

-- |
-- Module      : TRSConversion.Parse.Problem.MetaInfo
-- Description : Comment parser
--
-- This module defines parsers to parse the additional information (comment, author, etc.) of a
-- given rewriting system.
module TRSConversion.Parse.COPS.MetaInfo
  ( -- * COPS
    parseCopsMetaInfo,
  )
where

import Data.Char (isSpace)
import Data.Text (unpack)
import Text.Megaparsec (MonadParsec (notFollowedBy), between, noneOf, option, sepEndBy, some, takeWhile1P, takeWhileP, try, (<?>), (<|>), sepBy1, satisfy)
import Text.Megaparsec.Char (char, hspace, space, string, hspace1)

import TRSConversion.Parse.Utils (Parser)
import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..), emptyMetaInfo, mergeMetaInfo)

-- | Parser to extract comments as a @String@ from the (optional) @COMMENT@ block of the COPS TRS format.
--
parseCopsMetaInfo :: Parser MetaInfo
parseCopsMetaInfo = do
  ls <- sepEndBy (metaDoi <|> metaAuthors <|> metaComment) space
  return $ foldr mergeMetaInfo emptyMetaInfo ls
 where
   metaDoi = (\v -> emptyMetaInfo {doi = Just v}) <$> copsDoiLine
   metaAuthors = (\v -> emptyMetaInfo {submitted = Just v}) <$> copsAuthorsLine
   metaComment = (\v -> emptyMetaInfo {comment = Just [v]}) <$> copsCommentLine

copsDoiLine :: Parser String
copsDoiLine = "doi:" *> (pDoi <?> "doi")
  where
    pDoi = unpack <$> takeWhileP Nothing (not . isSpace)

copsAuthorsLine :: Parser [String]
copsAuthorsLine = "submitted by:" *> hspace *> (pAuthors <?> "authors")
  where
    pAuthors =
      sepBy1
        (some . try $ (char 'a' <* notFollowedBy (string "nd" <* hspace1)) <|> satisfy (\c -> c /= ',' && c /= '\n'))
        (some $ (string "," <|> string "and") <* hspace)

copsCommentLine :: Parser String
copsCommentLine = do
  pref <- unpack <$> takeWhile1P (Just "comment") (\c -> c `notElem` ['\n','\r','(',')'])
  rest <- option "" $ do
    par <- between (char '(') (char ')') parseComment
    suf <- option "" copsCommentLine
    pure $ '(' : par ++ ')': suf
  pure $ pref ++ rest


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

