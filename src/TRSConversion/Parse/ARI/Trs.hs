{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.ARI.Trs
Description : Parser for first-order TRSs

This module defines functions to parse first-order (MS-)TRSs in ARI format.
-}
module TRSConversion.Parse.ARI.Trs (
  -- ** ARI
  parseAriTrs,
)
where

import Data.Text (Text)
import TRSConversion.Parse.ARI.Rule (parseAriRule)
import TRSConversion.Parse.ARI.Sig (parseFsymArity)
import TRSConversion.Parse.ARI.Utils (ARIParser, keyword, sExpr, spaces)
import TRSConversion.Problem.Common.Rule (Rule)
import TRSConversion.Problem.Trs.Trs (Sig, Trs (..), TrsSig (..))
import Text.Megaparsec (many)

{- | Parse a first-order TRS in the provisional [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt).

Leading and trailing spaces are consumed: see the tests for more examples of the expected format.
Note that the entire input will not necessarily be consumed: use `<* eof` if this is needed.

qqjf I assumed that there is a fixed order of blocks: @meta-info@ then @format@ then @fun@ then @rule@.
-}
parseAriTrs :: ARIParser (Trs String String)
parseAriTrs = do
  spaces
  _ <- pFormat
  funSig <- pSignature
  rs <- pRules funSig
  return $
    Trs
      { rules = rs
      , signature = FunSig funSig
      }

pFormat :: ARIParser Text
pFormat = sExpr "format" (keyword "TRS")

pSignature :: ARIParser [Sig String]
pSignature = many (sExpr "fun" parseFsymArity)

pRules :: [Sig String] -> ARIParser [Rule String String]
pRules funSig = many (sExpr "rule" (parseAriRule funSig))
