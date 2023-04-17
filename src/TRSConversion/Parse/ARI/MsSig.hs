{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.Problem.MsSig
Description : Parser for MSTRS signatures

This module defines functions to parse an MSTRS signature into an 'MsSig'.
-}
module TRSConversion.Parse.ARI.MsSig (
  -- * ARI
  parseAriMsSig,
)
where

import TRSConversion.Parse.ARI.Utils (ARIParser, ident, parens, keyword)
import TRSConversion.Problem.MsTrs.MsSig (MsSig (..))
import Text.Megaparsec (some)

{- | Parser to extract the signature from a single @fun@ block of the ARI [MSTRS format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).
Expects a block like @fsym :sort (t1 ... tn t0)@ where the @t1@, ..., @tn@ are the input sorts of
@fsym@ and the final value @t0@ is the return sort of the function (see examples below).

Leading and trailing spaces are consumed.

>>> parseTest parseAriMsSig "add :sort (Nat List Nat))"
MsSig "add" (["Nat","List"], "Nat")

>>> parseTest parseAriMsSig "0 :sort (Nat)"
MsSig "0" ([], "Nat")
-}
parseAriMsSig :: ARIParser (MsSig String String)
parseAriMsSig = do
  fsym <- ident
  _ <- keyword ":sort"
  args <- parens (some ident)
  return $ MsSig fsym (take (length args - 1) args, last args)
