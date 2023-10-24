{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Formats.COPS.Unparse.UnparseMsTrs
-- Description : Unparser for MSTRSs
--
-- This module defines functions to output an 'MsTrs' in COPS and ARI format.
module TRSConversion.Formats.COPS.Unparse.UnparseMsTrs
  ( -- * COPS
    unparseCopsMsTrs,
  )
where

import qualified Data.IntMap as IntMap
import Prettyprinter (Doc, Pretty, vsep)

import TRSConversion.Problem.MsTrs.MsTrs (MsTrs (..))
import TRSConversion.Formats.COPS.Unparse.Problem.MsSig (unparseCopsMsSig)
import TRSConversion.Formats.COPS.Unparse.Problem.Rule (unparseCopsRules)
import TRSConversion.Unparse.Utils (filterEmptyDocs)


-- | Unparse a many-sorted TRS from the Haskell 'MsTrs' representation into
-- [COPS MSTRS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
--
-- Uses functions 'unparseCopsMsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
-- unparse each part of the 'MsTrs'.
unparseCopsMsTrs :: (Pretty f, Pretty v, Pretty s) => MsTrs f v s -> Either String (Doc ann)
unparseCopsMsTrs (MsTrs {rules = systems, signature = sig, numSystems = n, sorts = _})
  | n /= 1 = error "COPS format doesn't support MSTRSs with multiple systems"
  | otherwise =
      return $
        vsep (filterEmptyDocs [unparseCopsMsSig sig, unparseCopsRules rs])
  where
    rs = systems IntMap.! 1

