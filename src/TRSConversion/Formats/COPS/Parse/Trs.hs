{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Formats.COPS.Parse.Trs
Description : Parser for first-order TRSs

This module defines functions to parse a first-order TRS in COPS ARI format.
-}
module TRSConversion.Formats.COPS.Parse.Trs (
  -- ** COPS
  parseCopsTrs,
  parseCopsVarBlock,
)
where

import qualified Data.IntMap as IntMap
import TRSConversion.Formats.COPS.Parse.Rule (parseCopsTrsRules)
import TRSConversion.Formats.COPS.Parse.Sig (parseCopsSig)
import TRSConversion.Formats.COPS.Parse.Utils (COPSParser, block, ident)
import TRSConversion.Problem.Common.Rule (inferSigFromRules)
import TRSConversion.Problem.Trs.Trs (Trs (..), TrsSig (..))
import Text.Megaparsec (many, option, optional)

{- | Parse a first-order TRS in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php):
see the COCO website for details on the grammar and the tests for more examples.

Leading and trailing spaces are consumed.
Note that the entire input will not necessarily be consumed: : use `<* eof` if this is needed.
-}
parseCopsTrs :: COPSParser (Trs String String)
parseCopsTrs = do
  vs <- parseCopsVarBlock
  funSig <- optional (block "SIG" parseCopsSig)
  rs <- block "RULES" (parseCopsTrsRules vs)
  sig <- case funSig of
    Just fs -> pure $ FunSig fs
    Nothing ->
      case inferSigFromRules rs of
        Left err -> fail err
        Right fs -> pure $ FunSig fs
  return
    $ Trs
      { rules = IntMap.singleton 1 rs
      , signature = sig
      , numSystems = 1
      }

parseCopsVarBlock :: COPSParser [String]
parseCopsVarBlock = option [] $ block "VAR" (many ident)
