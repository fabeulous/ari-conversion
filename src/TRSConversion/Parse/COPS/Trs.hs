{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TRSConversion.Parse.COPS.Trs
-- Description : Parser for first-order TRSs
--
-- This module defines functions to parse a first-order TRS in COPS ARI format.
module TRSConversion.Parse.COPS.Trs
  ( -- ** COPS
    parseCopsTrs,
    parseCopsVarBlock
  )
where

import TRSConversion.Parse.COPS.Rule (parseCopsTrsRules)
import TRSConversion.Parse.COPS.Sig (parseCopsSig)
import TRSConversion.Parse.COPS.Utils (COPSParser, block, ident)
import TRSConversion.Problem.Trs.Trs (Trs (..), TrsSig (..))
import Text.Megaparsec (many, optional, option)
import qualified Data.IntMap as IntMap

-- | Parse a first-order TRS in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php):
-- see the COCO website for details on the grammar and the tests for more examples.
--
-- Leading and trailing spaces are consumed.
-- Note that the entire input will not necessarily be consumed: : use `<* eof` if this is needed.
parseCopsTrs :: COPSParser (Trs String String)
parseCopsTrs = do
  vs <- parseCopsVarBlock
  funSig <- optional (block "SIG" parseCopsSig)
  let trsSig = case funSig of
        Nothing -> Vars vs -- If no SIG block is given
        Just inputFunSig -> FullSig vs inputFunSig
  rs <- block "RULES" (parseCopsTrsRules trsSig)
  return $
    Trs
      { rules = IntMap.singleton 1 rs,
        signature = trsSig,
        numSystems = 1
      }

parseCopsVarBlock :: COPSParser [String]
parseCopsVarBlock = option [] $ block "VAR" (many ident)
