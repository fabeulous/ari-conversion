{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Formats.COPS.Parse.CSTrs
Description : Parser for CSTRSs ins COPS format
-}
module TRSConversion.Formats.COPS.Parse.CSTrs (
  -- * System
  parseCopsCSTrs,
  -- * Parsers
  pReplacementMap
)
where

import TRSConversion.Formats.COPS.Parse.Rule (parseCopsTrsRules)
import TRSConversion.Formats.COPS.Parse.Utils (COPSParser, block, ident, parens, symbol, naturalNumber)
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import Text.Megaparsec (many, option, some, sepBy)
import qualified Data.IntMap as IntMap
import TRSConversion.Problem.Common.Rule (inferSigFromRules)
import TRSConversion.Problem.Trs.TrsSig (TrsSig(FunSig))

{- | Parse a CSTRS in [COPS format](http://project-coco.uibk.ac.at/problems/cstrs.php):
see the COCO website for details on the grammar and the tests for more examples.
-}
parseCopsCSTrs :: COPSParser (CSTrs String String)
parseCopsCSTrs = do
  vs <- option [] $ block "VAR" (many ident)
  repMap <- block "REPLACEMENT-MAP" pReplacementMap
  rs <- block "RULES" (parseCopsTrsRules vs)
  sig <- case inferSigFromRules rs of
    Left err -> fail err
    Right fs -> pure $ FunSig fs
  return $
    CSTrs
      { rules = IntMap.singleton 1 rs
      , signature = sig
      , replacementMap = repMap
      , numSystems = 1
      }

pReplacementMap :: COPSParser (ReplacementMap String)
pReplacementMap = some replacementMapping

replacementMapping :: COPSParser (String, [Int])
replacementMapping = parens $ (,) <$> ident <*> sepBy naturalNumber (symbol ",")
