-- |
-- Module      : TRSConversion.Parse.Problem.Sig
-- Description : TRS signature parsers
--
-- This module defines functions to parse a 'Sig' from a @String@ input.
module TRSConversion.Parse.ARI.Sig
  ( parseFsymArity,
  )
where

import TRSConversion.Problem.Trs.Sig (Sig (..))
import Text.Megaparsec (takeWhile1P, (<?>))
import Data.Char (isDigit)
import TRSConversion.Parse.ARI.Utils (ARIParser, lexeme, ident)
import Data.Text (unpack)

-- | Parser to extract the function symbol and arity from a string @fsym int@ where int is
-- the arity of the given symbol (see example below). Leading and trailing spaces are removed.
--
-- Used for parsing TRSs in ARI format and the @SIG@ block of the COPS [extended TRS format](http://project-coco.uibk.ac.at/problems/trs.php#extended).
--
-- >>> parseTest parseCopsSig "fun 2"
-- Right (Sig "fun" 2)
parseFsymArity :: ARIParser (Sig String)
parseFsymArity = Sig <$> ident <*> naturalNumber

naturalNumber :: ARIParser Int
naturalNumber =
  lexeme (read . unpack <$> takeWhile1P (Just "digit") isDigit) <?> "natural number"
