{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{- |
Module      : Data.Conversion.Parse.ARI.CSTrs
Description : Parser for CSTRSs in ARI format
-}
module TRSConversion.Parse.ARI.CSCTrs (
  parseAriCSCTrs,
)
where

import TRSConversion.Parse.ARI.CTrs (pCRules, pCondType)
import TRSConversion.Parse.ARI.Utils (ARIParser, keyword, sExpr)
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs (..))
import TRSConversion.Problem.CTrs.CTrs (CTrs (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..))
import TRSConversion.Parse.ARI.CSTrs (pSignatureReplacementMap)

parseAriCSCTrs :: ARIParser (CSCTrs String String)
parseAriCSCTrs = do
  condType <- sExpr "format" (keyword "CSCTRS" *> pCondType)
  (sig, repMap) <- pSignatureReplacementMap
  rs <- pCRules sig
  return $
    CSCTrs
      { ctrs =
          CTrs
            { conditionType = condType
            , rules = rs
            , signature = FunSig sig
            }
      , replacementMap = repMap
      }
