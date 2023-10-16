{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Data.Conversion.Parse.ARI.CTrs
Description : Parser for first-order CTRSs

This module defines functions to parse a first-order CTRS in ARI format.
-}
module TRSConversion.Parse.ARI.CTrs (
  -- * System
  parseAriCTrs,
  parseAriCTrs',

  -- * Parsers
  pCSystems,
  pCRules,
  parseCondition,
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
import TRSConversion.Parse.ARI.Utils (ARIParser, indexOutOfRangeError, keyword, naturalNumber, sExpr, index, nonPositiveNumberError, FunSymb, VarSymb)
import TRSConversion.Problem.CTrs.CTrs (CRule (..), CTrs (..), CondType (..), Condition (..))
import TRSConversion.Problem.Common.Index (Index)
import TRSConversion.Problem.Trs.TrsSig (Sig, TrsSig (..))

parseAriCTrs :: ARIParser (CTrs FunSymb VarSymb)
parseAriCTrs = pFormat >>= uncurry parseAriCTrs'

parseAriCTrs' :: CondType -> Int -> ARIParser (CTrs FunSymb VarSymb)
parseAriCTrs' condType numSys = do
  -- (condType, numSys) <- pFormat
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
  numSys <- option 1 $ do
    _ <- keyword ":number"
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

pSignature :: ARIParser [Sig FunSymb]
pSignature = many parseAriSig

pCSystems :: Int -> [Sig FunSymb] -> ARIParser (IntMap [CRule FunSymb VarSymb])
pCSystems n sig = do
  indexedRules <- pCRules sig
  rls <- forM indexedRules $ \(i,r) -> do
    unless (Idx.index i <= n) $ registerParseError (indexOutOfRangeError n i)
    unless (Idx.index i > 0) $
      registerParseError (nonPositiveNumberError (Idx.index i) (Idx.startOffset i))
    pure (Idx.index i, r)
  let m = IntMap.fromListWith (++) [(i, [r]) | (i, r) <- rls]
  pure $ fmap reverse m -- reverse to preserve original order

pCRules :: [Sig FunSymb] -> ARIParser [(Index, CRule FunSymb VarSymb)]
pCRules funSig = many (sExpr "rule" (parseAriCRule funSig))

parseAriCRule :: [Sig FunSymb] -> ARIParser (Index, CRule FunSymb VarSymb)
parseAriCRule funSig = do
  o <- getOffset
  rule <- CRule <$> term <*> term <*> pConds
  idx <- option (Idx.Index 1 o) (keyword ":index" >> index)
  pure (idx, rule)
 where
  term = parsePrefixTerm funSig
  pConds = many (parseCondition funSig)

parseCondition :: [Sig FunSymb] -> ARIParser (Condition FunSymb VarSymb)
parseCondition funSig = sExpr "=" ((:==) <$> term <*> term)
  where
    term = parsePrefixTerm funSig
