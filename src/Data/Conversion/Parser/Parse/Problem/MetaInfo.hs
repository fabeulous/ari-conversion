{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parser.Parse.Problem.MetaInfo
-- Description : Comment parser
--
-- This module defines parser 'parseComment' to parse the comment of a given TRS.
module Data.Conversion.Parser.Parse.Problem.MetaInfo
  ( parseComment,
  )
where

import Data.Conversion.Parser.Parse.Utils (Parser)
import Text.Megaparsec
  ( noneOf,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (char)

-- | Parser to extract comments as a @String@ from a @COMMENT@ block of the COPS TRS format.
-- Calls itself recursively to allow for parentheses inside comments.
--
-- The recursive logic is adapted from the library @term-rewriting@.
-- qqjf should be updated to infer structure from comments (e.g. author, source, etc.).
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