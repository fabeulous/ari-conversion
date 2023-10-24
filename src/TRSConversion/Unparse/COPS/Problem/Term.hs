-- |
-- Module      : TRSConversion.Unparse.COPS.Problem.Term
-- Description : Unparser for terms
--
-- This module defines functions to unparse terms into prefix and applicative formats.
module TRSConversion.Unparse.COPS.Problem.Term
  ( -- * COPS
    unparseTerm,
  )
where

import TRSConversion.Problem.Common.Term (Term (..))
import Prettyprinter (Doc, Pretty, comma, emptyDoc, encloseSep, hsep, lparen, parens, pretty, rparen, (<+>))

-- | Unparse 'Term's using applicative notation:
-- see examples below and tests for more examples.
--
-- To pretty print term in prefix notation use 'unparsePrefixTerm'.
--
-- >>> unparseTerm [Var "x"]
-- x
--
-- >>> unparseTerm (Fun "f" [Var "x", Fun "g" [Var "y", Var "z"]])
-- f(x,g(y,z))
--
-- >>> unparseTerm (Fun "c" [])
-- c
unparseTerm :: (Pretty f, Pretty v) => Term f v -> Doc ann
unparseTerm (Var x) = pretty x
unparseTerm (Fun f ts) = pretty f <> args
  where
    args
      | null ts = emptyDoc -- Parse constants without parentheses
      | otherwise = encloseSep lparen rparen comma [unparseTerm t | t <- ts]

