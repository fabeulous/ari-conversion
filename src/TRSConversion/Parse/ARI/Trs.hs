{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : TRSConversion.Parse.ARI.Trs
Description : Parser for first-order TRSs

This module defines functions to parse first-order (MS-)TRSs in ARI format.
-}
module TRSConversion.Parse.ARI.Trs (
  -- ** ARI
  parseAriTrs,
  parseAriTrs',
  parseSystems,
)
where

import Control.Monad (forM, unless)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Megaparsec (MonadParsec (parseError), getOffset, many, option, registerParseError)

import TRSConversion.Parse.ARI.Rule (parseAriRule)
import TRSConversion.Parse.ARI.Sig (parseAriSig)
import TRSConversion.Parse.ARI.Utils (ARIParser, indexOutOfRangeError, keyword, naturalNumber, nonPositiveNumberError, sExpr)
import TRSConversion.Problem.Common.Index (Index (index))
import TRSConversion.Problem.Common.Rule (Rule)
import TRSConversion.Problem.Trs.Trs (Sig, Trs (..), TrsSig (..))
import qualified TRSConversion.Problem.Common.Index as Idx
import TRSConversion.Parse.Utils (Token)

{- | Parse a first-order TRS in the provisional [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt).

Leading and trailing spaces are consumed: see the tests for more examples of the expected format.
Note that the entire input will not necessarily be consumed: use `<* eof` if this is needed.

qqjf I assumed that there is a fixed order of blocks: @meta-info@ then @format@ then @fun@ then @rule@.
-}
parseAriTrs :: ARIParser (Trs (Token String) (Token String))
parseAriTrs = pFormat >>= parseAriTrs'

parseAriTrs' :: Int -> ARIParser (Trs (Token String) (Token String))
parseAriTrs' n = do
  funSig <- pSignature
  rs <- parseSystems n funSig
  return $
    Trs
      { rules = rs
      , signature = FunSig funSig
      , numSystems = n
      }

pFormat :: ARIParser Int
pFormat = sExpr "format" $ do
  _ <- keyword "TRS"
  n <- option 1 $ do
    _ <- keyword ":number"
    o <- getOffset
    n <- naturalNumber
    unless (n > 0) $ parseError (nonPositiveNumberError n o)
    pure n
  pure n

pSignature :: ARIParser [Sig (Token String)]
pSignature = many parseAriSig

parseSystems :: Int -> [Sig (Token String)] -> ARIParser (IntMap [Rule (Token String) (Token String)])
parseSystems numSys funSig = do
  indexedRules <- pRules funSig
  rls <- forM indexedRules $ \(i, r) -> do
    unless (index i <= numSys) $ registerParseError (indexOutOfRangeError numSys i)
    unless (index i > 0) $ registerParseError (nonPositiveNumberError (Idx.index i) (Idx.startOffset i))
    pure (index i, r)
  let m = IntMap.fromListWith (++) [(i, [r]) | (i, r) <- rls]
  pure $ fmap reverse m -- reverse to preserve original order

pRules :: [Sig (Token String)] -> ARIParser [(Index, Rule (Token String) (Token String))]
pRules funSig = many (parseAriRule funSig)
