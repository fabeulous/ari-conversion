{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Data.Conversion.Parser.Parse.Cops
-- Description : Parser for TRSs in COPS format
--
-- This module defines function 'parseCops' to parse a first-order TRS in COPS format.
module Data.Conversion.Parser.Parse.Cops
  ( parseCops,
  )
where

import Data.Conversion.Parser.Parse.Problem.MetaInfo (parseComment)
import Data.Conversion.Parser.Parse.Problem.Rule (parseRules)
import Data.Conversion.Parser.Parse.Problem.Sig (parseSig)
import Data.Conversion.Parser.Parse.Problem.Term (parseVariable)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, parens, stripSpaces)
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Text (pack)
import Text.Megaparsec
  ( many,
    optional,
    try,
    (<?>),
    (<|>),
  )
import Text.Megaparsec.Char (string)

-- | Parse a first-order TRS in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php):
-- see the COCO website for details on the grammar and allowed characters.
parseCops :: Parser (Trs String String)
parseCops = stripSpaces $ do
  vs <- try (block "VAR" (many parseVariable)) <|> return []
  inputSig <- optional (try $ block "SIG" parseSig)
  (rs, sig) <- block "RULES" (parseRules vs inputSig)
  metaInfo <- optional (block "COMMENT" parseComment)
  return
    ( Trs
        { rules = rs,
          variables = vs,
          signature = sig,
          comment = metaInfo
        }
    )
  where
    block :: String -> Parser a -> Parser a
    block name p =
      parens
        ( lexeme (string $ pack name) -- Parse block name
            *> lexeme p
        )
        <?> (name ++ " block")