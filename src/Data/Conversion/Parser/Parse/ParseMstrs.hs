-- |
-- Module      : Data.Conversion.Parser.Parse.ParseMstrs
-- Description : Functions to parse MSTRSs
--
-- This module defines functions to parse a many-sorted TRS in COPS and ARI format.
module Data.Conversion.Parser.Parse.ParseMstrs
  ( parseCopsMstrs,
  )
where

import Data.Conversion.Parser.Parse.Problem.MetaInfo (parseCopsMetaInfo) -- parseAriMetaInfo,
import Data.Conversion.Parser.Parse.Problem.MsSig (parseCopsMsSigs)
import Data.Conversion.Parser.Parse.Problem.Rule (parseCopsMstrsRules)
import Data.Conversion.Parser.Parse.Utils (Parser, parseBlock, stripSpaces)
import Data.Conversion.Problem.Common.MetaInfo (emptyMetaInfo)
import Data.Conversion.Problem.Mstrs.Mstrs (Mstrs (..))
-- import Data.Conversion.Problem.Trs.TrsSig (TrsSig)
import Data.Maybe (fromMaybe)
import Text.Megaparsec (optional)

-- | Parse a many-sorted TRS in [COPS format](http://project-coco.uibk.ac.at/problems/mstrs.php):
-- see the COCO website for details on the grammar and allowed characters and the tests for more examples.
--
-- Does not carry out type-checking for function applications: this should be handled by the user.
-- Currently just checks that the arities of applied functions are consistent.
--
-- Leading and trailing spaces are removed.
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