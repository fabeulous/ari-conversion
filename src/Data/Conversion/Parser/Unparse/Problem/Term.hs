-- |
-- Module      : Data.Conversion.Parser.Unparse.Problem.Term
-- Description : Unparser for terms
--
-- This module defines functions to unparse terms into prefix and applicative formats.
module Data.Conversion.Parser.Unparse.Problem.Term
  ( unparseTerm,
  )
where

import Data.Conversion.Problem.Common.Term (Term (..))
import Prettyprinter (Doc, Pretty, comma, emptyDoc, encloseSep, lparen, pretty, rparen)

-- | Unparse 'Term's using applicative notation (see examples below).
-- See 'unparsePrefixTerm' to pretty print term in prefix notation.
--
-- >>> unparseTerm [Var "x"]
-- x
--
-- >>> unparseTerm (Fun "f" [Var "x", Var "y"])
-- f(x,y)
--
-- >>> unparseTerm (Fun "c" [])
-- c
unparseTerm :: (Pretty f, Pretty v) => Term f v -> Doc ann
unparseTerm (Var x) = pretty x
unparseTerm (Fun f ts) = pretty f <> args
  where
    args
      | null ts = emptyDoc -- Parse constants without parentheses
      | otherwise = encloseSep lparen rparen comma [unparseTerm ti | ti <- ts]