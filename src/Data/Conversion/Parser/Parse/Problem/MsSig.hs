-- |
-- Module      : Data.Conversion.Parser.Parse.Problem.MsSig
-- Description : Parser for 'MsSig'
--
-- This module defines functions to parse an MSTRS signature from a @String@ input.
module Data.Conversion.Parser.Parse.Problem.MsSig
  ( parseCopsMsSigs,
    parseCopsMsSig,
  )
where

import Data.Conversion.Parser.Parse.Problem.Term (parseFunSymbol)
import Data.Conversion.Parser.Parse.Utils (Parser, lexeme, parens, stripSpaces)
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Data.Text (pack)
import Text.Megaparsec (many, some, (<?>))
import Text.Megaparsec.Char (spaceChar, string)

-- | Parser to extract the signature from a @SIG@ block of the COPS [MSTRS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
-- Expects a sequence of blocks @(fsym  t1 ... tn -> t)@ where @t1@,...,@tn@ are the input types of the function
-- and @t@ is the return type (see examples below).
-- Leading and trailing spaces are ignored.
--
-- >>> parse parseCopsMsSig "" (pack "(cons  Nat List -> List)")
-- Right [MsSig "cons" (["Nat","List"] "List")]
--
-- -- >>> parse parseCopsMsSigs "" (pack "(n  -> Nat)")
-- Right [MsSig "n" ([] "Nat")]
parseCopsMsSigs :: Parser [MsSig String String]
parseCopsMsSigs = stripSpaces $ many (parens parseCopsMsSig)

-- | Parser to extract the function symbol and arity from a string @fsym  t1 ... tn -> t@ where
-- @t1@,...,@tn@ are the input types of the function and @t@ is the return type (see examples below).
-- Leading and trailing spaces are removed.
--
-- Used for parsing the @SIG@ block of the COPS [MSTRS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
--
-- >>> parse parseCopsSig "" (pack "cons  Nat List -> List")
-- Right (MsSig "cons" (["Nat","List"] "List"))
parseCopsMsSig :: Parser (MsSig String String)
parseCopsMsSig = stripSpaces $ do
  fsym <- lexeme parseFunSymbol <?> "MsSig function symbol"
  inputSorts <- many (parseFunSymbol <* some spaceChar)
  _ <- lexeme (string $ pack "-> ") -- qqjf assume that sorts have the same constaints as function symbols
  outputSort <- lexeme parseFunSymbol
  return $ MsSig fsym (inputSorts, outputSort)
