{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TRSConversion.Parse.Problem.MsSig
Description : Parser for MSTRS signatures

This module defines functions to parse an MSTRS signature into an 'MsSig'.
-}
module TRSConversion.Formats.ARI.Parse.MsSig (
  -- * ARI
  parseAriMsSig,
)
where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set (Set)
import qualified Data.Set as Set
import TRSConversion.Formats.ARI.Parse.Utils (ARIParser, ident, sExpr, FunSymb, SortSymb)
import TRSConversion.Problem.MsTrs.MsSig (MsSig (..))
import Text.Megaparsec (getOffset, registerParseError, some, (<|>))
import qualified Text.Megaparsec.Error as E
import Data.Text (unpack)
import TRSConversion.Parse.Utils (tokenText)

{- | Parser to extract the signature from a single @fun@ block of the ARI [MSTRS format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).
Expects a block like @fsym :sort (t1 ... tn t0)@ where the @t1@, ..., @tn@ are the input sorts of
@fsym@ and the final value @t0@ is the return sort of the function (see examples below).

Leading and trailing spaces are consumed.

>>> parseTest parseAriMsSig "add :sort (Nat List Nat))"
MsSig "add" (["Nat","List"], "Nat")

>>> parseTest parseAriMsSig "0 :sort (Nat)"
MsSig "0" ([], "Nat")
-}
parseAriMsSig :: Set SortSymb -> ARIParser (MsSig FunSymb SortSymb)
parseAriMsSig declaredSorts = sExpr "fun" $ do
  fsym <- ident
  sorts <- sortP declaredSorts
  return $ MsSig fsym (init sorts, last sorts)

sortP :: Set SortSymb -> ARIParser [SortSymb]
sortP declaredSorts =
  (: []) <$> sortIdent <|> sExpr "->" ((:) <$> sortIdent <*> some sortIdent)
 where
  sortIdent :: ARIParser SortSymb
  sortIdent = do
    o <- getOffset
    name <- ident
    unless (name `Set.member` declaredSorts) $
      registerParseError (undeclaredSortError (unpack $ tokenText name) o)
    pure name

  undeclaredSortError name offset =
    E.TrivialError
      offset
      (Just . E.Tokens $ head name :| tail name)
      (Set.singleton (E.Label $ 'd' :| "eclared sort"))
