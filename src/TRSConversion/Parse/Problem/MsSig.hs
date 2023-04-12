{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TRSConversion.Parse.Problem.MsSig
-- Description : Parser for MSTRS signatures
--
-- This module defines functions to parse an MSTRS signature into an 'MsSig'.
module TRSConversion.Parse.Problem.MsSig
  ( -- * COPS
    parseCopsMsSigs,
    parseCopsMsSig,

    -- * ARI
    parseAriMsSig,
  )
where

import Control.Monad (guard)
import TRSConversion.Parse.Problem.Term (parseFunSymbol)
import TRSConversion.Parse.Utils (Parser, lexeme, parens, stripSpaces)
import TRSConversion.Problem.MsTrs.MsSig (MsSig (..))
import Text.Megaparsec (many, sepBy, some, (<?>))
import Text.Megaparsec.Char (spaceChar, string)

-- | Parser to extract the signature from a @SIG@ block of the COPS [MSTRS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
-- Expects a sequence of blocks @(fsym  t1 ... tn -> t)@ where @t1@, ..., @tn@ are the input sorts of the function
-- and @t@ is the return type (see examples below). Parses each block with 'parseCopsMsSig'.
--
-- Leading and trailing spaces are consumed.
--
-- >>> parseTest parseCopsMsSig "(cons  Nat List -> List)"
-- [MsSig "cons" (["Nat","List"], "List")]
--
-- >>> parseTest parseCopsMsSig "(n  -> Nat)"
-- [MsSig "n" ([], "Nat")]
parseCopsMsSigs :: Parser [MsSig String String]
parseCopsMsSigs = stripSpaces $ many (parens parseCopsMsSig)

-- | Parser to extract the function symbol and arity from a string @fsym  t1 ... tn -> t@ where
-- @t1@, ..., @tn@ are the input sorts of the function and @t@ is the return type (see example below).
-- Leading and trailing spaces are consumed.
--
-- Called in 'parseCopsMsSigs' to parse the @SIG@ block of the COPS [MSTRS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
--
-- >>> parseTest parseCopsSig "cons  Nat List -> List"
-- MsSig "cons" (["Nat","List"], "List")
parseCopsMsSig :: Parser (MsSig String String)
parseCopsMsSig = stripSpaces $ do
  fsym <- lexeme parseFunSymbol <?> "MsSig function symbol"
  inputSorts <- many (parseFunSymbol <* some spaceChar) -- qqjf assume that sorts have the same constraints as function symbols
  _ <- lexeme (string "-> ")
  outputSort <- lexeme parseFunSymbol
  return $ MsSig fsym (inputSorts, outputSort)

-- | Parser to extract the signature from a single @fun@ block of the ARI [MSTRS format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).
-- Expects a block like @fsym :sort (t1 ... tn t0)@ where the @t1@, ..., @tn@ are the input sorts of
-- @fsym@ and the final value @t0@ is the return sort of the function (see examples below).
--
-- Leading and trailing spaces are consumed.
--
-- >>> parseTest parseAriMsSig "add :sort (Nat List Nat))"
-- MsSig "add" (["Nat","List"], "Nat")
--
-- >>> parseTest parseAriMsSig "0 :sort (Nat)"
-- MsSig "0" ([], "Nat")
parseAriMsSig :: Parser (MsSig String String)
parseAriMsSig =
  stripSpaces $ do
    fsym <- lexeme parseFunSymbol <?> "MsSig function symbol"
    _ <- lexeme $ string ":sort "
    args <- lexeme $ parens (parseFunSymbol `sepBy` some spaceChar)
    guard (not $ null args)
    return $ MsSig fsym (take (length args - 1) args, last args)
