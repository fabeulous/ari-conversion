{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TRSConversion.Parse.COPS.MSTrs
-- Description : Functions to parse MSTRSs
--
-- This module defines functions to parse a many-sorted TRS in COPS and ARI format.
module TRSConversion.Parse.COPS.MSTrs
  ( -- ** COPS
    parseCopsMsTrs,
  )
where

import TRSConversion.Parse.Problem.MetaInfo (parseCopsMetaInfo)
import TRSConversion.Parse.Problem.MsSig (parseCopsMsSigs)
import TRSConversion.Parse.Problem.Rule (parseCopsMsTrsRules)
import TRSConversion.Parse.COPS.Utils (Parser, block)
import TRSConversion.Problem.Common.MetaInfo (emptyMetaInfo)
import TRSConversion.Problem.MsTrs.MsTrs (MsTrs (..))
import Data.Maybe (fromMaybe)
import Text.Megaparsec (optional)

-- | Parse a many-sorted TRS in [COPS format](http://project-coco.uibk.ac.at/problems/mstrs.php):
-- see the COCO website for details on the grammar and the tests for more examples.
--
-- Does not carry out type-checking for function applications: this should be handled by the user.
--
-- Leading and trailing spaces are consumed.
-- Note that the entire input will not necessarily be consumed: use `<* eof` if this is needed.
parseCopsMsTrs :: Parser (MsTrs String String String)
parseCopsMsTrs = do
  msSigs <- block "SIG" parseCopsMsSigs
  rs <- block "RULES" (parseCopsMsTrsRules msSigs)
  maybeMetaInfo <- optional (block "COMMENT" parseCopsMetaInfo)
  return $
    MsTrs
      { rules = rs,
        signature = msSigs,
        sorts = Nothing, -- Not set for COPS format
        metaInfo = fromMaybe emptyMetaInfo maybeMetaInfo
      }

