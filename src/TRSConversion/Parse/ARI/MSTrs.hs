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

import qualified Data.Set as Set
import Data.Text (Text)
import Text.Megaparsec (many, option)

import TRSConversion.Parse.ARI.MsSig (parseAriMsSig)
import TRSConversion.Parse.ARI.Trs (parseSystems)
import TRSConversion.Parse.ARI.Utils (ARIParser, ident, keyword, sExpr, naturalNumber)
import TRSConversion.Problem.MsTrs.MsTrs (MsSig (..), MsTrs (..))
import TRSConversion.Problem.Trs.Sig (Sig (..))

{- | Parse a many-sorted TRS in the provisional [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).

Leading and trailing spaces are consumed: see the tests for more examples of the expected format.
Note that the entire input will not necessarily be consumed: use `<* eof` if this is needed.

Currently no type checking is performed: this is left to the user.
It is also not checked whether the sorts used in function symbols align with explicity defined sorts.
Rules are parsed using 'parseAriRule' as in the untyped TRS setting.

qqjf I assumed that there is a fixed order of blocks: @meta-info@ then @format@ then @sort@ then @fun@ then @rule@.
-}
parseAriMsTrs :: ARIParser (MsTrs String String String)
parseAriMsTrs = do
  (_, numSys) <- pFormat "MSTRS"
  sortsList <- pSorts
  msSigs <- pMSSig (Set.fromList sortsList)
  rs <- parseSystems $ msSigToSigList msSigs
  return $
    MsTrs
      { rules = rs
      , signature = msSigs
      , sorts = Just sortsList
      , numSystems = numSys
      }
 where
  msSigToSigList :: [MsSig String String] -> [Sig String]
  msSigToSigList = map (\(MsSig fsym (inputSorts, _)) -> Sig fsym (length inputSorts))

pFormat :: Text -> ARIParser (Text, Int)
pFormat name = sExpr "format" $ do
  form <- keyword name
  numSys <- option 1 (keyword ":number" >> naturalNumber)
  pure (form, numSys)

pSorts :: ARIParser [String]
pSorts = many (sExpr "sort" ident)

pMSSig :: Set.Set String -> ARIParser [MsSig String String]
pMSSig declaredSorts = many (parseAriMsSig declaredSorts)
