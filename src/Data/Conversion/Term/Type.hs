module Data.Conversion.Term.Type
  ( 
    -- * 'Term' datatype re-exported from "Data.Rewriting.Term.Type"
    --
    -- | Has constructors 'Var' for variables and 'Fun' for function symbols.
    Term (..),
  )
where

import Data.Rewriting.Term.Type
import Prettyprinter

-- | Make 'Term' an instance of 'Pretty'
instance (Pretty f, Pretty v) => Pretty (Term f v) where
    pretty (Var x) = pretty x
    pretty (Fun f ts) = pretty f <> args where
        args = encloseSep lparen rparen comma [pretty ti | ti <- ts]