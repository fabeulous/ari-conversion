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
import Data.Text (Text)
import Data.Void (Void)
import TRSConversion.Parse.COPS.MetaInfo (parseCopsMetaInfo)
import TRSConversion.Parse.COPS.Term (parseTermVars)
import TRSConversion.Parse.COPS.Utils (block, ident, keyword, symbol)
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition (..))
import TRSConversion.Problem.Common.MetaInfo (emptyMetaInfo)
import TRSConversion.Problem.Trs.TrsSig (TrsSig (Vars))
import Text.Megaparsec (Parsec, many, option, sepBy1, (<|>))

type Parser = Parsec Void Text

parseCopsCTrs :: Parser (CTrs String String)
parseCopsCTrs = do
  condType <- pCondTypeBlock
  vars <- option [] pVarsBlock
  rs <- pRulesBlock vars
  metaInf <- option emptyMetaInfo (block "COMMENT" parseCopsMetaInfo)
  return $
    CTrs
      { conditionType = condType
      , rules = rs
      , signature = TRSConversion.Problem.Trs.TrsSig.Vars vars
      , metaInfo = metaInf
      }

pCondType :: Parser CondType
pCondType =
  (keyword "ORIENTED" $> Oriented)
    <|> (keyword "JOIN" $> Join)
    <|> (keyword "SEMI-EQUATIONAL" $> SemiEquational)

pCondTypeBlock :: Parser CondType
pCondTypeBlock = block "CONDITIONTYPE" pCondType

pVarsBlock :: Parser [String]
pVarsBlock = block "VAR" pVars
 where
  pVars = many ident

pRulesBlock :: [String] -> Parser [CRule String String]
pRulesBlock vars = block "RULES" pRules
 where
  pRules = many (pCRule vars)

pCRule :: [String] -> Parser (CRule String String)
pCRule vars = CRule <$> parseTermVars vars <*> (symbol "->" *> parseTermVars vars) <*> pConditions
 where
  pConditions =
    option [] $ symbol "|" *> sepBy1 pCondition (symbol ",")

  pCondition = (:==) <$> parseTermVars vars <*> (symbol "==" *> parseTermVars vars)
