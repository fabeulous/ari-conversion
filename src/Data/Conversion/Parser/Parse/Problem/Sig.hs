-- |
-- Module      : Data.Conversion.Parser.Parse.Problem.Sig
-- Description : Signature parser
--
-- This module defines functions to parse a TRS signature from a @String@ input.
module Data.Conversion.Parser.Parse.Problem.Sig
  ( parseCopsSig,
    parseFsymArity,
  )
where

import Data.Conversion.Parser.Parse.Problem.Term (parseFunSymbol)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, parens, stripSpaces)
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Text.Megaparsec (many, some)
import Text.Megaparsec.Char (digitChar, spaceChar)

-- | Parser to extract the signature from a @SIG@ block of the COPS [extended TRS format](http://project-coco.uibk.ac.at/problems/trs.php#extended).
-- Expects a sequence of blocks @(fsym Int)@ where the @Int@ is the arity of the given symbol (see example below).
-- Leading and trailing spaces are ignored.
--
-- >>> parse parseCopsSig "" (pack "(f 2) (a 0) (h 1)")
-- Right [Sig "f" 2,Sig "a" 0,Sig "h" 1]
parseCopsSig :: Parser [Sig String]
parseCopsSig = stripSpaces $ many (parens parseFsymArity)

-- | Parser to extract the function symbol and arity from a string @fsym int@ where int is
-- the arity of the given symbol (see example below). Leading and trailing spaces are removed.
--
-- Used for parsing TRSs in ARI format and the @SIG@ block of the COPS [extended TRS format](http://project-coco.uibk.ac.at/problems/trs.php#extended).
--
-- >>> parse parseCopsSig "" (pack "fun 2")
-- Right (Sig "fun" 2)
parseFsymArity :: Parser (Sig String)
parseFsymArity = stripSpaces $ do
  fsym <- lexeme parseFunSymbol
  arity <- read <$> some digitChar
  return $ Sig fsym arity
