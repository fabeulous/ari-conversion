{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TRSConversion.Parse.COPS.Trs
-- Description : Parser for first-order TRSs
--
-- This module defines functions to parse a first-order TRS in COPS ARI format.
module TRSConversion.Parse.COPS.Trs
  ( -- ** COPS
    parseCopsTrs,
  )
where

import TRSConversion.Parse.Problem.MetaInfo (parseCopsMetaInfo)
import TRSConversion.Parse.Problem.Rule (parseCopsTrsRules)
import TRSConversion.Parse.Problem.Sig (parseCopsSig)
import TRSConversion.Parse.Problem.Term (parseVariable)
import TRSConversion.Parse.COPS.Utils (Parser, lexeme, block,)
import TRSConversion.Problem.Common.MetaInfo (emptyMetaInfo)
import TRSConversion.Problem.Trs.Trs (Trs (..), TrsSig (..))
import Data.Maybe (fromMaybe)
import Text.Megaparsec (many, optional, option)

-- | Parse a first-order TRS in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php):
-- see the COCO website for details on the grammar and the tests for more examples.
--
-- Leading and trailing spaces are consumed.
-- Note that the entire input will not necessarily be consumed: : use `<* eof` if this is needed.
parseCopsTrs :: Parser (Trs String String)
parseCopsTrs = do
  vs <- option [] $ block "VAR" (many $ lexeme parseVariable)
  funSig <- optional (block "SIG" parseCopsSig)
  let trsSig = case funSig of
        Nothing -> Vars vs -- If no SIG block is given
        Just inputFunSig -> FullSig vs inputFunSig
  rs <- block "RULES" (parseCopsTrsRules trsSig)
  maybeMetaInfo <- optional (block "COMMENT" parseCopsMetaInfo)
  return $
    Trs
      { rules = rs,
        signature = trsSig,
        metaInfo = fromMaybe emptyMetaInfo maybeMetaInfo
      }

