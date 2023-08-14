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

import Control.Monad (forM, unless)
import Data.Functor (($>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified TRSConversion.Problem.Common.Index as Idx
import Text.Megaparsec (getOffset, many, option, registerParseError, (<|>), parseError)

import TRSConversion.Parse.ARI.Sig (parseAriSig)
import TRSConversion.Parse.ARI.Term (parsePrefixTerm)
import TRSConversion.Parse.ARI.Utils (ARIParser, indexOutOfRangeError, keyword, naturalNumber, sExpr, index, nonPositiveNumberError)
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition (..))
import TRSConversion.Problem.Common.Index (Index)
import TRSConversion.Problem.Trs.TrsSig (Sig, TrsSig (..))

parseAriCTrs :: ARIParser (CTrs String String)
parseAriCTrs = do
  (condType, numSys) <- pFormat
  sig <- pSignature
  rs <- pCSystems numSys sig
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
  _ <- keyword ":number"
  numSys <- option 1 $ do
    o <- getOffset
    n <- naturalNumber
    unless (n > 0) $ parseError (nonPositiveNumberError n o)
    pure n
  pure (condType, numSys)

pCondType :: ARIParser CondType
pCondType =
  (keyword "oriented" $> Oriented)
    <|> (keyword "join" $> Join)
    <|> (keyword "semi-equational" $> SemiEquational)

pSignature :: ARIParser [Sig String]
pSignature = many parseAriSig

pCSystems :: Int -> [Sig String] -> ARIParser (IntMap [CRule String String])
pCSystems n sig = do
  indexedRules <- pCRules sig
  rls <- forM indexedRules $ \(i,r) -> do
    unless (Idx.index i <= n) $ registerParseError (indexOutOfRangeError n i)
    pure (Idx.index i, r)
  let m = IntMap.fromListWith (++) [(i, [r]) | (i, r) <- rls]
  pure $ fmap reverse m -- reverse to preserve original order

pCRules :: [Sig String] -> ARIParser [(Index, CRule String String)]
pCRules funSig = many (sExpr "rule" (parseAriCRule funSig))

parseAriCRule :: [Sig String] -> ARIParser (Index, CRule String String)
parseAriCRule funSig = do
  o <- getOffset
  rule <- CRule <$> term <*> term <*> pConds
  idx <- option (Idx.Index 1 o) (keyword ":index" >> index)
  pure (idx, rule)
 where
  term = parsePrefixTerm funSig
  pConds = many pCond
  pCond = sExpr "=" ((:==) <$> term <*> term)
