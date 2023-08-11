{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Conversion.Parse.COPS.CSCTrs
Description : Parser for CSCTRSs
-}
module TRSConversion.Parse.COPS.CSCTrs (
  -- ** COPS
  parseCopsCSCTrs,
)
where

import TRSConversion.Parse.COPS.Utils (COPSParser, block, ident)
import TRSConversion.Problem.CTrs.CTrs (CTrs (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig(Vars))
import Text.Megaparsec (many, option)
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs(..))
import TRSConversion.Parse.COPS.CTrs (pCondTypeBlock, pCRulesBlock)
import TRSConversion.Parse.COPS.CSTrs (pReplacementMap)
import qualified Data.IntMap as IntMap

parseCopsCSCTrs :: COPSParser (CSCTrs String String)
parseCopsCSCTrs = do
  condType <- pCondTypeBlock
  vs <- option [] $ block "VAR" (many ident)
  repMap <- block "REPLACEMENT-MAP" pReplacementMap
  let trsSig = Vars vs
  rs <- pCRulesBlock vs
  return $
    CSCTrs
      { ctrs =
         CTrs { conditionType = condType
              , rules = IntMap.singleton 1 rs
              , signature = trsSig
              , numSystems = 1
              }
      , replacementMap = repMap
      }
