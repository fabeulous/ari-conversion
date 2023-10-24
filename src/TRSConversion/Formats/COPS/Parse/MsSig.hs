{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.Problem.MsSig
Description : Parser for MSTRS signatures

This module defines functions to parse an MSTRS signature into an 'MsSig'.
-}
module TRSConversion.Formats.COPS.Parse.MsSig (
  -- * COPS
  parseCopsMsSigs,
  parseCopsMsSig,
)
where

import TRSConversion.Formats.COPS.Parse.Utils (COPSParser, ident, parens, symbol)
import TRSConversion.Problem.MsTrs.MsSig (MsSig (..))
import Text.Megaparsec (many, (<?>))

{- | Parser to extract the signature from a @SIG@ block of the COPS [MSTRS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
Expects a sequence of blocks @(fsym  t1 ... tn -> t)@ where @t1@, ..., @tn@ are the input sorts of the function
and @t@ is the return type (see examples below). Parses each block with 'parseCopsMsSig'.

Leading and trailing spaces are consumed.

>>> parseTest parseCopsMsSig "(cons  Nat List -> List)"
[MsSig "cons" (["Nat","List"], "List")]

>>> parseTest parseCopsMsSig "(n  -> Nat)"
[MsSig "n" ([], "Nat")]
-}
parseCopsMsSigs :: COPSParser [MsSig String String]
parseCopsMsSigs = many (parens parseCopsMsSig)

{- | Parser to extract the function symbol and arity from a string @fsym  t1 ... tn -> t@ where
@t1@, ..., @tn@ are the input sorts of the function and @t@ is the return type (see example below).
Leading and trailing spaces are consumed.

Called in 'parseCopsMsSigs' to parse the @SIG@ block of the COPS [MSTRS format](http://project-coco.uibk.ac.at/problems/mstrs.php).

>>> parseTest parseCopsSig "cons  Nat List -> List"
MsSig "cons" (["Nat","List"], "List")
-}
parseCopsMsSig :: COPSParser (MsSig String String)
parseCopsMsSig = do
  fsym <- ident <?> "MsSig function symbol"
  inputSorts <- many ident
  _ <- symbol "-> "
  outputSort <- ident
  return $ MsSig fsym (inputSorts, outputSort)
