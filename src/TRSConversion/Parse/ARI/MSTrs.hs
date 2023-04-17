{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.ARI.MsTrs
Description : Functions to parse MSTRSs

This module defines functions to parse a many-sorted TRS in ARI format.
-}
module TRSConversion.Parse.ARI.MSTrs (
  -- ** ARI
  parseAriMsTrs,
)
where

import Data.Text (Text)
import TRSConversion.Parse.ARI.Utils (Parser, keyword, sExpr, ident)
import TRSConversion.Parse.ARI.MsSig (parseAriMsSig)
import TRSConversion.Parse.ARI.Rule (parseAriRule)
import TRSConversion.Problem.Common.Rule (Rule)
import TRSConversion.Problem.MsTrs.MsTrs (MsSig (..), MsTrs (..))
import TRSConversion.Problem.Trs.Sig (Sig (..))
import Text.Megaparsec (many)

{- | Parse a many-sorted TRS in the provisional [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).

Leading and trailing spaces are consumed: see the tests for more examples of the expected format.
Note that the entire input will not necessarily be consumed: use `<* eof` if this is needed.

Currently no type checking is performed: this is left to the user.
It is also not checked whether the sorts used in function symbols align with explicity defined sorts.
Rules are parsed using 'parseAriRule' as in the untyped TRS setting.

qqjf I assumed that there is a fixed order of blocks: @meta-info@ then @format@ then @sort@ then @fun@ then @rule@.
-}
parseAriMsTrs :: Parser (MsTrs String String String)
parseAriMsTrs = do
  _ <- pFormat "MSTRS"
  sortsList <- pSorts
  msSigs <- pMSSig
  rs <- pRules $ msSigToSigList msSigs
  return $
    MsTrs
      { rules = rs
      , signature = msSigs
      , sorts = Just sortsList
      }
 where
  msSigToSigList :: [MsSig String String] -> [Sig String]
  msSigToSigList = map (\(MsSig fsym (inputSorts, _)) -> Sig fsym (length inputSorts))

pFormat :: Text -> Parser Text
pFormat name = sExpr "format" (keyword name)

pSorts :: Parser [String]
pSorts = many (sExpr "sort" ident)

pMSSig :: Parser [MsSig String String]
pMSSig = many (sExpr "fun " parseAriMsSig)

pRules :: [Sig String] -> Parser [Rule String String]
pRules sig = many (sExpr "rule " (parseAriRule sig))
