module Data.Conversion.Rule.Type
  ( 
    -- * Rule datatype copied almost verbatim from "Data.Rewriting.Rule.Type"
    --
    -- | ...
    Rule (..),
  )
where


import Data.Conversion.Term.Type
import Prettyprinter

-- | Rewrite rule with left-hand side and right-hand side.
data Rule f v = Rule { lhs :: Term f v, rhs :: Term f v }
    deriving (Ord, Eq, Show)

-- | Make 'Rule' an instance of 'Pretty'
instance (Pretty f, Pretty v) => Pretty (Rule f v) where
    pretty (Rule l r) = hang 2 $ pretty l <+> pretty "->" <+> pretty r