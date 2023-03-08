-- |
-- Module      : Data.Conversion.Parser.Unparse.Problem.Term
-- Description : Unparser for terms
--
-- This module defines functions to unparse terms into prefix and applicative formats.
module Data.Conversion.Parser.Unparse.Problem.Term
  ( unparseTerm,
    unparsePrefixTerm,
  )
where

import Data.Conversion.Problem.Common.Term (Term (..))
import Prettyprinter (Doc, Pretty, comma, emptyDoc, encloseSep, hsep, lparen, parens, pretty, rparen, (<+>))

-- | Unparse 'Term's using applicative notation:
-- see examples below and tests for more examples.
-- Use 'unparsePrefixTerm' to pretty print term in prefix notation.
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

-- | Unparse 'Term's into prefix applicative notation:
-- see examples below and tests for more examples.
-- Use 'unparseTerm' to pretty print term in applicative notation.
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
    unparseArgs (Fun f ts) = case ts of
      [] -> pretty f -- Constant
      [t1] -> pretty f <+> unparseArgs t1 -- No parentheses for 1 argument
      manyTerms -> parens (pretty f <+> unparseTerms manyTerms)