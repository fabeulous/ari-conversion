{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parse.ParseTrs
-- Description : Parser for first-order TRSs
--
-- This module defines functions to parse a first-order TRS in COPS and ARI format.
module Data.Conversion.Parse.ParseTrs
  ( -- ** COPS
    parseCopsTrs,

    -- ** ARI
    parseAriTrs,
  )
where

import Data.Conversion.Parse.Problem.MetaInfo (parseAriMetaInfo, parseCopsMetaInfo)
import Data.Conversion.Parse.Problem.Rule (parseAriRule, parseCopsTrsRules)
import Data.Conversion.Parse.Problem.Sig (parseCopsSig, parseFsymArity)
import Data.Conversion.Parse.Problem.Term (parseVariable)
import Data.Conversion.Parse.Utils (Parser, lexeme, parseBlock, stripSpaces)
import Data.Conversion.Problem.Common.MetaInfo (emptyMetaInfo)
import Data.Conversion.Problem.Trs.Trs (Trs (..), TrsSig (..))
import Data.Maybe (fromMaybe)
import Text.Megaparsec (many, optional, try, (<|>))
import Text.Megaparsec.Char (string)

-- | Parse a first-order TRS in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php):
-- see the COCO website for details on the grammar and the tests for more examples.
--
-- Leading and trailing spaces are consumed.
-- Note that the entire input will not necessarily be consumed: : use `<* eof` if this is needed.
parseCopsTrs :: Parser (Trs String String)
parseCopsTrs = stripSpaces $ do
  vs <- try (parseBlock "VAR" (many $ lexeme parseVariable)) <|> return []
  funSig <- optional (try $ parseBlock "SIG" parseCopsSig)
  let trsSig = case funSig of
        Nothing -> Vars vs -- If no SIG block is given
        Just inputFunSig -> FullSig vs inputFunSig
  rs <- parseBlock "RULES" (parseCopsTrsRules trsSig)
  maybeMetaInfo <- optional (parseBlock "COMMENT" parseCopsMetaInfo)
  return $
    Trs
      { rules = rs,
        signature = trsSig,
        metaInfo = fromMaybe emptyMetaInfo maybeMetaInfo
      }

-- | Parse a first-order TRS in the provisional [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt).
--
-- Leading and trailing spaces are consumed: see the tests for more examples of the expected format.
-- Note that the entire input will not necessarily be consumed: use `<* eof` if this is needed.
--
-- qqjf I assumed that there is a fixed order of blocks: @meta-info@ then @format@ then @fun@ then @rule@.
parseAriTrs :: Parser (Trs String String)
parseAriTrs = stripSpaces $ do
  trsMetaInfo <- parseAriMetaInfo
  _ <- parseBlock "format" (string "TRS")
  funSig <- many (try $ parseBlock "fun " parseFsymArity)
  rs <- many (try $ parseBlock "rule " (parseAriRule funSig))
  return $
    Trs
      { rules = rs,
        signature = FunSig funSig,
        metaInfo = trsMetaInfo
      }
