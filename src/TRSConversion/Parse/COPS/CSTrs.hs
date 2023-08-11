{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.COPS.CSTrs
Description : Parser for CSTRSs ins COPS format
-}
module TRSConversion.Parse.COPS.CSTrs (
  -- * System
  parseCopsCSTrs,
  -- * Parsers
  pReplacementMap
)
where

import TRSConversion.Parse.COPS.Rule (parseCopsTrsRules)
import TRSConversion.Parse.COPS.Utils (COPSParser, block, ident, parens, symbol, naturalNumber)
import TRSConversion.Problem.CSTrs.CSTrs (CSTrs (..), ReplacementMap)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..))
import Text.Megaparsec (many, option, some, sepBy)
import qualified Data.IntMap as IntMap

{- | Parse a CSTRS in [COPS format](http://project-coco.uibk.ac.at/problems/cstrs.php):
see the COCO website for details on the grammar and the tests for more examples.
-}
parseCopsCSTrs :: COPSParser (CSTrs String String)
parseCopsCSTrs = do
  vs <- option [] $ block "VAR" (many ident)
  repMap <- block "REPLACEMENT-MAP" pReplacementMap
  let trsSig = Vars vs
  rs <- block "RULES" (parseCopsTrsRules trsSig)
  return $
    CSTrs
      { rules = IntMap.singleton 1 rs
      , signature = trsSig
      , replacementMap = repMap
      , numSystems = 1
      }

pReplacementMap :: COPSParser (ReplacementMap String)
pReplacementMap = some replacementMapping

replacementMapping :: COPSParser (String, [Int])
replacementMapping = parens $ (,) <$> ident <*> sepBy naturalNumber (symbol ",")
