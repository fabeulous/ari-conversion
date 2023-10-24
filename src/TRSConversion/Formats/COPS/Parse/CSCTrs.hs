{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Conversion.Formats.COPS.Parse.CSCTrs
Description : Parser for CSCTRSs
-}
module TRSConversion.Formats.COPS.Parse.CSCTrs (
  -- ** COPS
  parseCopsCSCTrs,
)
where

import qualified Data.IntMap as IntMap
import TRSConversion.Formats.COPS.Parse.CSTrs (pReplacementMap)
import TRSConversion.Formats.COPS.Parse.CTrs (pCRulesBlock, pCondTypeBlock)
import TRSConversion.Formats.COPS.Parse.Utils (COPSParser, block, ident)
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs (..))
import TRSConversion.Problem.CTrs.CTrs (CTrs (..), inferSigFromRules)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (FunSig))
import Text.Megaparsec (many, option)

parseCopsCSCTrs :: COPSParser (CSCTrs String String)
parseCopsCSCTrs = do
  condType <- pCondTypeBlock
  vs <- option [] $ block "VAR" (many ident)
  repMap <- block "REPLACEMENT-MAP" pReplacementMap
  rs <- pCRulesBlock vs
  sig <- case inferSigFromRules rs of
    Left err -> fail err
    Right fs -> pure $ FunSig fs
  return
    $ CSCTrs
      { ctrs =
          CTrs
            { conditionType = condType
            , rules = IntMap.singleton 1 rs
            , signature = sig
            , numSystems = 1
            }
      , replacementMap = repMap
      }
