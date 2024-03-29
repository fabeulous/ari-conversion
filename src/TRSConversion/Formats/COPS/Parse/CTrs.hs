{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Conversion.Parse.ParseTrs
Description : Parser for first-order TRSs

This module defines functions to parse a first-order TRS in COPS and ARI format.
-}
module TRSConversion.Formats.COPS.Parse.CTrs (
  -- * system
  parseCopsCTrs,
  -- * parsers
  pCondTypeBlock,
  parseCopsCondition,
  pCRulesBlock
)
where

import Data.Functor (($>))
import qualified Data.IntMap as IntMap
import Text.Megaparsec (many, option, sepBy1, (<|>))

import TRSConversion.Formats.COPS.Parse.Term (parseTermVars)
import TRSConversion.Formats.COPS.Parse.Trs (parseCopsVarBlock)
import TRSConversion.Formats.COPS.Parse.Utils (COPSParser, block, keyword, symbol)
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition (..), inferSigFromRules)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (FunSig))

parseCopsCTrs :: COPSParser (CTrs String String)
parseCopsCTrs = do
  condType <- pCondTypeBlock
  vars <- parseCopsVarBlock
  rs <- pCRulesBlock vars
  case inferSigFromRules rs of
    Left err -> fail err
    Right fs ->
      return
        $ CTrs
          { conditionType = condType
          , rules = IntMap.singleton 1 rs
          , signature = FunSig fs
          , numSystems = 1
          }

pCondType :: COPSParser CondType
pCondType =
  (keyword "ORIENTED" $> Oriented)
    <|> (keyword "JOIN" $> Join)
    <|> (keyword "SEMI-EQUATIONAL" $> SemiEquational)

pCondTypeBlock :: COPSParser CondType
pCondTypeBlock = block "CONDITIONTYPE" pCondType

pCRulesBlock :: [String] -> COPSParser [CRule String String]
pCRulesBlock vars = block "RULES" pRules
 where
  pRules = many (pCRule vars)

pCRule :: [String] -> COPSParser (CRule String String)
pCRule vars = CRule <$> parseTermVars vars <*> (symbol "->" *> parseTermVars vars) <*> pConditions
 where
  pConditions =
    option [] $ symbol "|" *> sepBy1 (parseCopsCondition vars) (symbol ",")

parseCopsCondition :: [String] -> COPSParser (Condition String String)
parseCopsCondition vars = (:==) <$> parseTermVars vars <*> (symbol "==" *> parseTermVars vars)
