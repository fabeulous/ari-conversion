{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.ARI.Trs
Description : Parser for first-order TRSs

This module defines functions to parse first-order (MS-)TRSs in ARI format.
-}
module TRSConversion.Parse.ARI.Trs (
  -- ** ARI
  parseAriTrs,
  parseSystems,
)
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import Text.Megaparsec (many, option)

import TRSConversion.Parse.ARI.Rule (parseAriRule)
import TRSConversion.Parse.ARI.Sig (parseAriSig)
import TRSConversion.Parse.ARI.Utils (ARIParser, keyword, naturalNumber, sExpr, spaces)
import TRSConversion.Problem.Common.Rule (Rule)
import TRSConversion.Problem.Trs.Trs (Sig, Trs (..), TrsSig (..))

{- | Parse a first-order TRS in the provisional [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt).

Leading and trailing spaces are consumed: see the tests for more examples of the expected format.
Note that the entire input will not necessarily be consumed: use `<* eof` if this is needed.

qqjf I assumed that there is a fixed order of blocks: @meta-info@ then @format@ then @fun@ then @rule@.
-}
parseAriTrs :: ARIParser (Trs String String)
parseAriTrs = do
  spaces
  (_, n) <- pFormat
  funSig <- pSignature
  rs <- parseSystems funSig
  return $
    Trs
      { rules = rs
      , signature = FunSig funSig
      , numSystems = n
      }

pFormat :: ARIParser (Text, Int)
pFormat = sExpr "format"
  ((,) <$> keyword "TRS" <*> option 1 (keyword ":number" >> naturalNumber))

pSignature :: ARIParser [Sig String]
pSignature = many parseAriSig

parseSystems :: [Sig String] -> ARIParser (IntMap [Rule String String])
parseSystems funSig = do
  indexedRules <- pRules funSig
  let m = IntMap.fromListWith (++) [(i, [r]) | (i,r) <- indexedRules]
  pure $ fmap reverse m -- reverse to preserve original order


pRules :: [Sig String] -> ARIParser [(Int, Rule String String)]
pRules funSig = many (parseAriRule funSig)
