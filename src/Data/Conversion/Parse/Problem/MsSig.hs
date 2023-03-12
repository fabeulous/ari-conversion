-- |
-- Module      : Data.Conversion.Parse.Problem.MsSig
-- Description : Parser for 'MsSig'
--
-- This module defines functions to parse an MSTRS signature from a @String@ input.
module Data.Conversion.Parse.Problem.MsSig
  ( parseCopsMsSigs,
    parseCopsMsSig,
    parseAriMsSig,
  )
where

import Control.Monad (guard)
import Data.Conversion.Parse.Problem.Term (parseFunSymbol)
import Data.Conversion.Parse.Utils (Parser, lexeme, parens, stripSpaces)
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Data.Text (pack)
import Text.Megaparsec (many, sepBy, some, (<?>))
import Text.Megaparsec.Char (spaceChar, string)

-- | Parser to extract the signature from a @SIG@ block of the COPS [MSTRS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
-- Expects a sequence of blocks @(fsym  t1 ... tn -> t)@ where @t1@,...,@tn@ are the input types of the function
-- and @t@ is the return type (see examples below).
--
-- Leading and trailing spaces are consumed.
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
-- Leading and trailing spaces are consumed.
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

-- | Parser to extract the signature from a @fun@ block of the ARI [MSTRS format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).
-- Expects a of blocks like @fsym :sort (t1 ... tn t0)@ where the @t1@,...,@tn@ are the input sorts of
-- @fsym@ and the final sort @t0@ is the return type of the function.
--
-- Leading and trailing spaces are consumed.
--
-- >>> parse parseAriMsSig "" (pack "+ :sort (Nat Nat Nat))")
-- Right [MsSig "+" (["Nat","Nat"] "Nat")]
--
-- -- >>> parse parseAriMsSig "" (pack "0 :sort (Nat)")
-- Right [MsSig "0" ([] "Nat")]
parseAriMsSig :: Parser (MsSig String String)
parseAriMsSig =
  stripSpaces $ do
    fsym <- lexeme parseFunSymbol <?> "MsSig function symbol"
    _ <- lexeme $ string (pack ":sort ")
    args <- lexeme $ parens (parseFunSymbol `sepBy` some spaceChar)
    guard (not $ null args)
    return $ MsSig fsym (take (length args - 1) args, last args)