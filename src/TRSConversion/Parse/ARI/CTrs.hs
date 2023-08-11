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
  pCSystems,
  pCRules,
  pCondType,
)
where

import Data.Functor (($>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Megaparsec (many, option, (<|>))

import TRSConversion.Parse.ARI.Sig (parseAriSig)
import TRSConversion.Parse.ARI.Term (parsePrefixTerm)
import TRSConversion.Parse.ARI.Utils (ARIParser, keyword, naturalNumber, sExpr)
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition (..))
import TRSConversion.Problem.Trs.TrsSig (Sig, TrsSig (..))

parseAriCTrs :: ARIParser (CTrs String String)
parseAriCTrs = do
  (condType, numSys) <- pFormat
  sig <- pSignature
  rs <- pCSystems sig
  return $
    CTrs
      { conditionType = condType
      , rules = rs
      , signature = FunSig sig
      , numSystems = numSys
      }

pFormat :: ARIParser (CondType, Int)
pFormat = sExpr "format" $ do
  _ <- keyword "CTRS"
  condType <- pCondType
  numSys <- option 1 (keyword ":number" >> naturalNumber)
  pure (condType, numSys)

pCondType :: ARIParser CondType
pCondType =
  (keyword "oriented" $> Oriented)
    <|> (keyword "join" $> Join)
    <|> (keyword "semi-equational" $> SemiEquational)

pSignature :: ARIParser [Sig String]
pSignature = many parseAriSig

pCSystems :: [Sig String] -> ARIParser (IntMap [CRule String String])
pCSystems sig = do
  indexedRules <- pCRules sig
  let m = IntMap.fromListWith (++) [(i, [r]) | (i, r) <- indexedRules]
  pure $ fmap reverse m -- reverse to preserve original order

pCRules :: [Sig String] -> ARIParser [(Int, CRule String String)]
pCRules funSig = many (sExpr "rule" (parseAriCRule funSig))

parseAriCRule :: [Sig String] -> ARIParser (Int, CRule String String)
parseAriCRule funSig = do
  rule <- CRule <$> term <*> term <*> pConds
  index <- option 1 (keyword ":index" >> naturalNumber)
  pure (index, rule)
 where
  term = parsePrefixTerm funSig
  pConds = many pCond
  pCond = sExpr "=" ((:==) <$> term <*> term)
