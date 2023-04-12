-- |
-- Module      : TRSConversion.Unparse.Problem.Term
-- Description : Unparser for terms
--
-- This module defines functions to unparse terms into prefix and applicative formats.
module TRSConversion.Unparse.Problem.Term
  ( -- * COPS
    unparseTerm,

    -- * ARI
    unparsePrefixTerm,
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

-- | Unparse 'Term's into prefix notation:
-- see examples below and tests for more examples.
--
-- To pretty print term in applicative notation use 'unparseTerm'.
--
-- >>> unparsePrefixTerm [Var "x"]
-- x
--
-- >>> unparsePrefixTerm (Fun "f" [Var "x", Fun "g" [Var "y", Var "z"]])
-- f x (g y z)
--
-- >>> unparsePrefixTerm (Fun "c" [])
-- c
unparsePrefixTerm :: (Pretty f, Pretty v) => Term f v -> Doc ann
unparsePrefixTerm (Var x) = pretty x
unparsePrefixTerm (Fun fsym []) = pretty fsym -- Constant
unparsePrefixTerm (Fun fsym args) = pretty fsym <+> unparseTerms args
  where
    -- Unparse a list of terms, adding parentheses to nested terms
    unparseTerms :: (Pretty f, Pretty v) => [Term f v] -> Doc ann
    unparseTerms ts = hsep $ map unparseArgs ts
    -- Unparse a nested term, adding parentheses and spaces where needed.
    unparseArgs :: (Pretty f, Pretty v) => Term f v -> Doc ann
    unparseArgs (Var x) = pretty x
    unparseArgs (Fun f ts)
      | null ts = pretty f -- Constant (no spaces or parentheses)
      | otherwise = parens (pretty f <+> unparseTerms ts)
