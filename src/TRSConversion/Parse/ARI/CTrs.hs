{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Conversion.Parse.ARI.CTrs
Description : Parser for first-order CTRSs

This module defines functions to parse a first-order CTRS in ARI format.
-}
module TRSConversion.Parse.ARI.CTrs (
  -- * System
  parseAriCTrs,
  -- * Parsers
  pCRules,
  pCondType,
)
where

import Data.Functor (($>))
import TRSConversion.Parse.ARI.Sig (parseAriSig)
import TRSConversion.Parse.ARI.Term (parsePrefixTerm)
import TRSConversion.Parse.ARI.Utils (ARIParser, keyword, sExpr)
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition (..))
import TRSConversion.Problem.Trs.TrsSig (Sig, TrsSig (..))
import Text.Megaparsec (many, (<|>))

parseAriCTrs :: ARIParser (CTrs String String)
parseAriCTrs = do
  condType <- pFormat
  sig <- pSignature
  rs <- pCRules sig
  return $
    CTrs
      { conditionType = condType
      , rules = rs
      , signature = FunSig sig
      }

pFormat :: ARIParser CondType
pFormat = sExpr "format" (keyword "CTRS" *> pCondType)

pCondType :: ARIParser CondType
pCondType =
  (keyword "oriented" $> Oriented)
    <|> (keyword "join" $> Join)
    <|> (keyword "semi-equational" $> SemiEquational)

pSignature :: ARIParser [Sig String]
pSignature = many parseAriSig

pCRules :: [Sig String] -> ARIParser [CRule String String]
pCRules funSig = many (sExpr "rule" (parseAriCRule funSig))

parseAriCRule :: [Sig String] -> ARIParser (CRule String String)
parseAriCRule funSig = CRule <$> term <*> term <*> pConds
 where
  term = parsePrefixTerm funSig
  pConds = many pCond
  pCond = sExpr "=" ((:==) <$> term <*> term)
