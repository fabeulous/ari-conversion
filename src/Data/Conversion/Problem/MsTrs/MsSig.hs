-- |
-- Module      : Data.Conversion.Problem.MsTrs.MsSig
-- Description : Type definition for MSTRS signature
--
-- This module contains the 'MsSig' type definition for specifying the signature of a many-sorted
-- term rewriting system and helper functions.
module Data.Conversion.Problem.MsTrs.MsSig
  ( -- * Many-sorted signature datatype
    MsSig (..),

    -- * Helper functions
    inferSorts,
  )
where

import Data.List (nub)

-- | Datatype for the signature of a single function symbol in a many-sorted TRS ('MsTrs').
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

-- | Function to infer a list of all unique sorts in an 'MsSig'.
--
-- >>> inferSorts [MsSig "cons" (["Nat","List"] "List"), MsSig "treeList" (["Tree"] "List")]
-- ["Nat", "List", "Tree"]
inferSorts :: Eq s => [MsSig f s] -> [s]
inferSorts = nub . concatMap (\(MsSig _ (inSorts, outSort)) -> outSort : inSorts)