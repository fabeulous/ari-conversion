-- |
-- Module      : Data.Conversion.Problem.Mstrs.MsSig
-- Description : Type definition for MSTRS signature
--
-- This module contains the 'MsSig' type definition for specifying the signature of a many-sorted
-- term rewriting system.
module Data.Conversion.Problem.Mstrs.MsSig
  ( MsSig (..),
  )
where

-- | Datatype for the signature of a single function symbol in a many-sorted TRS ('Mstrs').
--
-- For example, a function symbol @cons@ which takes a @Nat@ and @List@ and returns a @List@ may be written as
-- @MsSig "cons" (["Nat"], "List")@. A constant @n@ of type @Nat@ may be written as @MsSig "n" ([], "Nat")@.
data MsSig f s
  = MsSig
      f
      -- ^ The function symbol
      ([s], s)
      -- ^ A list of the input types and the single output type of the function symbol
  deriving (Ord, Eq, Show)