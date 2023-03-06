{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parser.Parse.ParseTrs
-- Description : Parser for TRSs in COPS format
--
-- This module defines function 'parseCops' to parse a first-order TRS in COPS format.
module Data.Conversion.Parser.Parse.ParseTrs
  ( parseCops,
    parseAri,
  )
where

import Control.Monad (guard)
import Data.Conversion.Parser.Parse.Problem.MetaInfo (parseComment)
import Data.Conversion.Parser.Parse.Problem.Rule (parseAriRule, parseCopsRules)
import Data.Conversion.Parser.Parse.Problem.Sig (parseCopsSig, parseFsymArity)
import Data.Conversion.Parser.Parse.Problem.Term (parseVariable)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, parseBlock, stripSpaces)
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Text.Megaparsec
  ( many,
    optional,
    try,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar)

-- | Parse a first-order TRS in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php):
-- see the COCO website for details on the grammar and allowed characters and the tests for more examples.
--
-- Leading and trailing spaces are removed.
parseCops :: Parser (Trs String String)
parseCops = stripSpaces $ do
  vs <- try (parseBlock "VAR" (many $ lexeme parseVariable)) <|> return []
  funSig <- optional (try $ parseBlock "SIG" parseCopsSig)
  let trsSig = case funSig of
        Nothing -> Vars vs -- If no SIG block is given
        Just inputFunSig -> FullSig vs inputFunSig
  rs <- parseBlock "RULES" (parseCopsRules trsSig)
  metaInfo <- optional (parseBlock "COMMENT" parseComment)
  return
    ( Trs
        { rules = rs,
          signature = trsSig,
          comment = metaInfo
        }
    )

-- | Parse a first-order TRS in the provisional [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt)
-- and the tests for more examples.
-- qqjf I assume that the order of blocks is @meta-info@ then @format@ then @fun@ then @rule@.
--
-- Leading and trailing spaces are removed.
parseAri :: Parser (Trs String String)
parseAri = stripSpaces $ do
  metaInfo <- many (try $ parseBlock "meta-info" parseComment)
  format <- parseBlock "format" (many alphaNumChar)
  guard (format == "TRS") -- Assert correct format
  funSig <- many (try $ parseBlock "fun " parseFsymArity)
  rs <- many (try $ parseBlock "rule " (parseAriRule funSig))
  return
    ( Trs
        { rules = rs,
          signature = FunSig funSig,
          comment = if null metaInfo then Nothing else Just $ concat metaInfo -- qqjf
        }
    )
