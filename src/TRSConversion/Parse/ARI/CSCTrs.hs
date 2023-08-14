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

import Text.Megaparsec (option)

import TRSConversion.Parse.ARI.CSTrs (pSignatureReplacementMap)
import TRSConversion.Parse.ARI.CTrs (pCSystems, pCondType)
import TRSConversion.Parse.ARI.Utils (ARIParser, keyword, naturalNumber, sExpr)
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs (..))
import TRSConversion.Problem.CTrs.CTrs (CTrs (..), CondType)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..))

parseAriCSCTrs :: ARIParser (CSCTrs String String)
parseAriCSCTrs = do
  (condType, n) <- pFormat
  (sig, repMap) <- pSignatureReplacementMap
  rs <- pCSystems n sig
  return $
    CSCTrs
      { ctrs =
          CTrs
            { conditionType = condType
            , rules = rs
            , signature = FunSig sig
            , numSystems = n
            }
      , replacementMap = repMap
      }

pFormat :: ARIParser (CondType, Int)
pFormat = sExpr "format" $ do
  condType <- keyword "CSCTRS" *> pCondType
  n <- option 1 (keyword ":number" >> naturalNumber)
  pure (condType, n)
