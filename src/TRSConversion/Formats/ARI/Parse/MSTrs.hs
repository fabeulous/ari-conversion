{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Formats.ARI.Parse.MsTrs
Description : Functions to parse MSTRSs

This module defines functions to parse a many-sorted TRS in ARI format.
-}
module TRSConversion.Formats.ARI.Parse.MSTrs (
  -- ** ARI
  parseAriMsTrs,
  parseAriMsTrs',
)
where

import qualified Data.Set as Set
import Data.Text (Text)
import Text.Megaparsec (many, option)

import TRSConversion.Formats.ARI.Parse.MsSig (parseAriMsSig)
import TRSConversion.Formats.ARI.Parse.Trs (parseSystems)
import TRSConversion.Formats.ARI.Parse.Utils (ARIParser, ident, keyword, sExpr, naturalNumber, restrictedIdent, FunSymb, VarSymb, SortSymb)
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
parseAriMsTrs :: ARIParser (MsTrs FunSymb VarSymb SortSymb)
parseAriMsTrs = pFormat "MSTRS" >>= parseAriMsTrs'

parseAriMsTrs' :: Int -> ARIParser (MsTrs FunSymb VarSymb SortSymb)
parseAriMsTrs' numSys = do
  sortsList <- pSorts
  msSigs <- pMSSig (Set.fromList sortsList)
  rs <- parseSystems numSys $ msSigToSigList msSigs
  return $
    MsTrs
      { rules = rs
      , signature = msSigs
      , sorts = Just sortsList
      , numSystems = numSys
      }
 where
  msSigToSigList :: [MsSig FunSymb SortSymb] -> [Sig FunSymb]
  msSigToSigList = map (\(MsSig fsym (inputSorts, _)) -> Sig fsym (length inputSorts))

pFormat :: Text -> ARIParser Int
pFormat name = sExpr "format" $ do
  _ <- keyword name
  option 1 (keyword ":number" >> naturalNumber)

pSorts :: ARIParser [SortSymb]
pSorts = many (sExpr "sort" restrictedIdent)

pMSSig :: Set.Set SortSymb -> ARIParser [MsSig FunSymb SortSymb]
pMSSig declaredSorts = many (parseAriMsSig declaredSorts)
