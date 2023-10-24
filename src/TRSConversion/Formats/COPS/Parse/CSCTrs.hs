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

import qualified Data.IntMap as IntMap
import TRSConversion.Parse.COPS.CSTrs (pReplacementMap)
import TRSConversion.Parse.COPS.CTrs (pCRulesBlock, pCondTypeBlock)
import TRSConversion.Parse.COPS.Utils (COPSParser, block, ident)
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
