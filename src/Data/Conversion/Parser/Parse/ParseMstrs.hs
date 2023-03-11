-- |
-- Module      : Data.Conversion.Parser.Parse.ParseMstrs
-- Description : Functions to parse MSTRSs
--
-- This module defines functions to parse a many-sorted TRS in COPS and ARI format.
module Data.Conversion.Parser.Parse.ParseMstrs
  ( parseCopsMstrs,
    parseAriMstrs,
  )
where

import Data.Conversion.Parser.Parse.Problem.MetaInfo (parseAriMetaInfo, parseCopsMetaInfo)
import Data.Conversion.Parser.Parse.Problem.MsSig (parseAriMsSig, parseCopsMsSigs)
import Data.Conversion.Parser.Parse.Problem.Rule (parseAriRule, parseCopsMstrsRules)
import Data.Conversion.Parser.Parse.Problem.Term (parseFunSymbol)
import Data.Conversion.Parser.Parse.Utils (Parser, parseBlock, stripSpaces)
import Data.Conversion.Problem.Common.MetaInfo (emptyMetaInfo)
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Data.Conversion.Problem.Mstrs.Mstrs (Mstrs (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Text.Megaparsec (many, optional, try)
import Text.Megaparsec.Char (string)

-- | Parse a many-sorted TRS in [COPS format](http://project-coco.uibk.ac.at/problems/mstrs.php):
-- see the COCO website for details on the grammar and allowed characters and the tests for more examples.
--
-- Does not carry out type-checking for function applications: this should be handled by the user.
--
-- Leading and trailing spaces are consumed.
parseCopsMstrs :: Parser (Mstrs String String String)
parseCopsMstrs = stripSpaces $ do
  msSigs <- parseBlock "SIG" parseCopsMsSigs
  rs <- parseBlock "RULES" (parseCopsMstrsRules msSigs)
  maybeMetaInfo <- optional (parseBlock "COMMENT" parseCopsMetaInfo)
  return $
    Mstrs
      { rules = rs,
        signature = msSigs,
        metaInfo = fromMaybe emptyMetaInfo maybeMetaInfo
      }

-- | Parse a many-sorted TRS in the provisional [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt)
-- Leading and trailing spaces are consumed. See the tests for more examples of the expected format.
--
-- Currently no type checking is performed: this is left to the user.
-- Rules are parsed using 'parseAriRule' as in the untyped TRS setting.
--
-- qqjf I assume that there is a fixed order of blocks: @meta-info@ then @format@ then @sort@ then @fun@ then @rule@.
parseAriMstrs :: Parser (Mstrs String String String)
parseAriMstrs = stripSpaces $ do
  mstrsMetaInfo <- parseAriMetaInfo
  _ <- parseBlock "format" (string $ pack "MSTRS")
  sorts <- many (try $ parseBlock "sort " parseFunSymbol) -- qqjf assumed sorts have same constraints as fun symbols
  msSigs <- many (try $ parseBlock "fun " parseAriMsSig)
  rs <- many (try $ parseBlock "rule " (parseAriRule $ msSigToSigList msSigs))
  return $
    Mstrs
      { rules = rs,
        signature = msSigs,
        metaInfo = mstrsMetaInfo
      }
  where
    msSigToSigList :: [MsSig String String] -> [Sig String]
    msSigToSigList = map (\(MsSig fsym (inputSorts, _)) -> Sig fsym (length inputSorts))
