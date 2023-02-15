module Data.Conversion.Problem.Sig.Type
  ( 
    -- * Untyped Signature datatype
    Sig (..),
  )
where


import Data.Conversion.Problem.Term.Type
import Prettyprinter



-- | Datatype for an untyped function symbol signature
-- To do: make into an abstract datatype?
data Sig f = Sig {fsym :: f, arity :: Int}
    deriving (Ord, Eq, Show)

-- | Make 'Sig' an instance of 'Pretty'
instance (Pretty f) => Pretty (Sig f) where
    pretty (Sig fsym arity) = pretty fsym <+> pretty "/" <+> pretty arity