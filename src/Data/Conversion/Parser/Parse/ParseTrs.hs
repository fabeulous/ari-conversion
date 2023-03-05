{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parser.Parse.ParseTrs
-- Description : Parser for TRSs in COPS format
--
-- This module defines function 'parseCops' to parse a first-order TRS in COPS format.
module Data.Conversion.Parser.Parse.ParseTrs
  ( parseCops,
  )
where

import Data.Conversion.Parser.Parse.Problem.MetaInfo (parseComment)
import Data.Conversion.Parser.Parse.Problem.Rule (parseRules)
import Data.Conversion.Parser.Parse.Problem.Sig (parseSig)
import Data.Conversion.Parser.Parse.Problem.Term (parseVariable)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, parseBlock, stripSpaces)
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Text.Megaparsec
  ( many,
    optional,
    try,
    (<|>),
  )

-- | Parse a first-order TRS in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php):
-- see the COCO website for details on the grammar and allowed characters.
parseCops :: Parser (Trs String String)
parseCops = stripSpaces $ do
  vs <- try (parseBlock "VAR" (many $ lexeme parseVariable)) <|> return []
  funSig <- optional (try $ parseBlock "SIG" parseSig)
  let trsSig = case funSig of
        Nothing -> Vars vs -- If no SIG block is given
        Just inputFunSig -> FullSig vs inputFunSig
  rs <- parseBlock "RULES" (parseRules trsSig)
  metaInfo <- optional (parseBlock "COMMENT" parseComment)
  return
    ( Trs
        { rules = rs,
          variables = vs,
          signature = trsSig,
          comment = metaInfo
        }
    )