{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Conversion.Parse.ARI.CTrs
Description : Parser for first-order CTRSs

This module defines functions to parse a first-order CTRS in ARI format.
-}
module TRSConversion.Parse.ARI.CTrs (
  -- ** COPS
  parseAriCTrs,
)
where

import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import TRSConversion.Parse.ARI.Sig (parseFsymArity)
import TRSConversion.Parse.ARI.Term (parsePrefixTerm)
import TRSConversion.Parse.ARI.Utils (keyword, parens, sExpr, symbol)
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition (..))
import TRSConversion.Problem.Trs.TrsSig (Sig, TrsSig (..))
import Text.Megaparsec (Parsec, many, option, sepBy, (<|>))

type Parser = Parsec Void Text

parseAriCTrs :: Parser (CTrs String String)
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

pFormat :: Parser CondType
pFormat = sExpr "format" (keyword "CTRS" *> pCondType)

pCondType :: Parser CondType
pCondType =
  (keyword "oriented" $> Oriented)
    <|> (keyword "join" $> Join)
    <|> (keyword "semi-equational" $> SemiEquational)

pSignature :: Parser [Sig String]
pSignature = many (sExpr "fun" parseFsymArity)

pCRules :: [Sig String] -> Parser [CRule String String]
pCRules funSig = many (sExpr "rule" (parseAriCRule funSig))

parseAriCRule :: [Sig String] -> Parser (CRule String String)
parseAriCRule funSig = CRule <$> term <*> term <*> pConds
 where
  term = parsePrefixTerm funSig
  pConds = option [] (keyword ":condition" *> parens (sepBy pCond (symbol ",")))
  pCond = sExpr "=" ((:==) <$> term <*> term)
