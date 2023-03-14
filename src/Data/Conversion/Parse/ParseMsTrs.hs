{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parse.ParseMsTrs
-- Description : Functions to parse MSTRSs
--
-- This module defines functions to parse a many-sorted TRS in COPS and ARI format.
module Data.Conversion.Parse.ParseMsTrs
  ( -- ** COPS
    parseCopsMsTrs,

    -- ** ARI
    parseAriMsTrs,
  )
where

import Data.Conversion.Parse.Problem.MetaInfo (parseAriMetaInfo, parseCopsMetaInfo)
import Data.Conversion.Parse.Problem.MsSig (parseAriMsSig, parseCopsMsSigs)
import Data.Conversion.Parse.Problem.Rule (parseAriRule, parseCopsMsTrsRules)
import Data.Conversion.Parse.Problem.Term (parseFunSymbol)
import Data.Conversion.Parse.Utils (Parser, parseBlock, stripSpaces)
import Data.Conversion.Problem.Common.MetaInfo (emptyMetaInfo)
import Data.Conversion.Problem.MsTrs.MsTrs (MsSig (..), MsTrs (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Maybe (fromMaybe)
import Text.Megaparsec (many, optional, try)
import Text.Megaparsec.Char (string)

-- | Parse a many-sorted TRS in [COPS format](http://project-coco.uibk.ac.at/problems/mstrs.php):
-- see the COCO website for details on the grammar and the tests for more examples.
--
-- Does not carry out type-checking for function applications: this should be handled by the user.
--
-- Leading and trailing spaces are consumed.
parseCopsMsTrs :: Parser (MsTrs String String String)
parseCopsMsTrs = stripSpaces $ do
  msSigs <- parseBlock "SIG" parseCopsMsSigs
  rs <- parseBlock "RULES" (parseCopsMsTrsRules msSigs)
  maybeMetaInfo <- optional (parseBlock "COMMENT" parseCopsMetaInfo)
  return $
    MsTrs
      { rules = rs,
        signature = msSigs,
        sorts = Nothing, -- Not set for COPS format
        metaInfo = fromMaybe emptyMetaInfo maybeMetaInfo
      }

-- | Parse a many-sorted TRS in the provisional [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).
--
-- Leading and trailing spaces are consumed: see the tests for more examples of the expected format.
--
-- Currently no type checking is performed: this is left to the user.
-- It is also not checked whether the sorts used in function symbols align with explicity defined sorts.
-- Rules are parsed using 'parseAriRule' as in the untyped TRS setting.
--
-- qqjf I assumed that there is a fixed order of blocks: @meta-info@ then @format@ then @sort@ then @fun@ then @rule@.
parseAriMsTrs :: Parser (MsTrs String String String)
parseAriMsTrs = stripSpaces $ do
  mstrsMetaInfo <- parseAriMetaInfo
  _ <- parseBlock "format" (string "MSTRS")
  sortsList <- many (try $ parseBlock "sort " parseFunSymbol) -- qqjf assumed sorts have same constraints as fun symbols
  msSigs <- many (try $ parseBlock "fun " parseAriMsSig)
  rs <- many (try $ parseBlock "rule " (parseAriRule $ msSigToSigList msSigs))
  return $
    MsTrs
      { rules = rs,
        signature = msSigs,
        sorts = Just sortsList,
        metaInfo = mstrsMetaInfo
      }
  where
    msSigToSigList :: [MsSig String String] -> [Sig String]
    msSigToSigList = map (\(MsSig fsym (inputSorts, _)) -> Sig fsym (length inputSorts))
