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
import Data.Conversion.Parser.Parse.Utils (Parser, parseBlock, stripSpaces)
import Data.Conversion.Problem.Trs.Trs (Trs (..))
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
  vs <- try (parseBlock "VAR" (many parseVariable)) <|> return []
  inputSig <- optional (try $ parseBlock "SIG" parseSig)
  (rs, sig) <- parseBlock "RULES" (parseRules vs inputSig)
  metaInfo <- optional (parseBlock "COMMENT" parseComment)
  return
    ( Trs
        { rules = rs,
          variables = vs,
          signature = sig,
          comment = metaInfo
        }
    )