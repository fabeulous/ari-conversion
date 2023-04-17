{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Conversion.Parse.ParseTrs
Description : Parser for first-order TRSs

This module defines functions to parse a first-order TRS in COPS and ARI format.
-}
module TRSConversion.Parse.COPS.CTrs (
  -- ** COPS
  parseCopsCTrs,
)
where

import Data.Functor (($>))
import TRSConversion.Parse.COPS.Term (parseTermVars)
import TRSConversion.Parse.COPS.Utils (COPSParser, block, ident, keyword, symbol)
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition (..))
import TRSConversion.Problem.Trs.TrsSig (TrsSig(Vars))
import Text.Megaparsec (many, option, sepBy1, (<|>))

parseCopsCTrs :: COPSParser (CTrs String String)
parseCopsCTrs = do
  condType <- pCondTypeBlock
  vars <- option [] pVarsBlock
  rs <- pRulesBlock vars
  return $
    CTrs
      { conditionType = condType
      , rules = rs
      , signature = Vars vars
      }

pCondType :: COPSParser CondType
pCondType =
  (keyword "ORIENTED" $> Oriented)
    <|> (keyword "JOIN" $> Join)
    <|> (keyword "SEMI-EQUATIONAL" $> SemiEquational)

pCondTypeBlock :: COPSParser CondType
pCondTypeBlock = block "CONDITIONTYPE" pCondType

pVarsBlock :: COPSParser [String]
pVarsBlock = block "VAR" pVars
 where
  pVars = many ident

pRulesBlock :: [String] -> COPSParser [CRule String String]
pRulesBlock vars = block "RULES" pRules
 where
  pRules = many (pCRule vars)

pCRule :: [String] -> COPSParser (CRule String String)
pCRule vars = CRule <$> parseTermVars vars <*> (symbol "->" *> parseTermVars vars) <*> pConditions
 where
  pConditions =
    option [] $ symbol "|" *> sepBy1 pCondition (symbol ",")

  pCondition = (:==) <$> parseTermVars vars <*> (symbol "==" *> parseTermVars vars)
