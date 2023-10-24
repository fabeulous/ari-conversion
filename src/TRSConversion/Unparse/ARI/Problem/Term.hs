-- |
-- Module      : TRSConversion.Unparse.Problem.Term
-- Description : Unparser for terms
--
-- This module defines functions to unparse terms into prefix and applicative formats.
module TRSConversion.Unparse.Problem.Term
  ( -- * ARI
    unparsePrefixTerm,
  )
where

import TRSConversion.Problem.Common.Term (Term (..))
import Prettyprinter (Doc, Pretty, comma, emptyDoc, encloseSep, hsep, lparen, parens, pretty, rparen, (<+>))

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
unparsePrefixTerm (Fun fsym args) = parens (pretty fsym <+> unparseTerms args)
  where
    -- Unparse a list of terms, adding parentheses to nested terms
    unparseTerms :: (Pretty f, Pretty v) => [Term f v] -> Doc ann
    unparseTerms ts = hsep $ map unparsePrefixTerm ts
