{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Conversion.Formats.ARI.Parse.CSTrs
Description : Parser for CSTRSs in ARI format
-}
module TRSConversion.Formats.ARI.Parse.CSCTrs (
  parseAriCSCTrs,
  parseAriCSCTrs'
)
where

import Text.Megaparsec (option)

import TRSConversion.Formats.ARI.Parse.CSTrs (pSignatureReplacementMap)
import TRSConversion.Formats.ARI.Parse.CTrs (pCSystems, pCondType)
import TRSConversion.Formats.ARI.Parse.Utils (ARIParser, keyword, naturalNumber, sExpr, FunSymb, VarSymb)
import TRSConversion.Problem.CSCTrs.CSCTrs (CSCTrs (..))
import TRSConversion.Problem.CTrs.CTrs (CTrs (..), CondType)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (..))

parseAriCSCTrs :: ARIParser (CSCTrs FunSymb VarSymb)
parseAriCSCTrs = pFormat >>= uncurry parseAriCSCTrs'

parseAriCSCTrs' :: CondType -> Int -> ARIParser (CSCTrs FunSymb VarSymb)
parseAriCSCTrs' condType n = do
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
  _ <- keyword "CSCTRS"
  condType <- pCondType
  n <- option 1 (keyword ":number" >> naturalNumber)
  pure (condType, n)
