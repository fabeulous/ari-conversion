{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.Problem.Sig
Description : TRS signature parsers

This module defines functions to parse a 'Sig' from a @String@ input.
-}
module TRSConversion.Parse.ARI.Sig (
  parseFsymArity,
  parseAriSig,
)
where

import Data.Char (isDigit)
import Data.Text (unpack)
import TRSConversion.Parse.ARI.Utils (ARIParser, ident, lexeme, sExpr, restrictedIdent, FunSymb)
import TRSConversion.Problem.Trs.Sig (Sig (..))
import Text.Megaparsec (takeWhile1P, (<?>))

parseAriSig :: ARIParser (Sig FunSymb)
parseAriSig = sExpr "fun" parseFsymArity

{- | Parser to extract the function symbol and arity from a string @fsym int@ where int is
the arity of the given symbol (see example below). Leading and trailing spaces are removed.

Used for parsing TRSs in ARI format and the @SIG@ block of the COPS [extended TRS format](http://project-coco.uibk.ac.at/problems/trs.php#extended).

>>> parseTest parseCopsSig "fun 2"
Right (Sig "fun" 2)
-}
parseFsymArity :: ARIParser (Sig FunSymb)
parseFsymArity = Sig <$> restrictedIdent <*> naturalNumber

naturalNumber :: ARIParser Int
naturalNumber =
  lexeme (read . unpack <$> takeWhile1P (Just "digit") isDigit) <?> "natural number"
