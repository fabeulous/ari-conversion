-- |
-- Module      : TRSConversion.Formats.COPS.Unparse.UnparseTrs
-- Description : Unparser for TRSs
--
-- This module defines functions to output a 'Trs' in COPS and ARI format.
module TRSConversion.Formats.COPS.Unparse.UnparseTrs
  ( -- * COPS
    unparseCopsTrs,
  )
where

import Control.Monad (forM)
import Data.Foldable (toList)
import Prettyprinter (Doc, Pretty, vsep)

import TRSConversion.Problem.Trs.Trs (Trs (..))
import TRSConversion.Formats.COPS.Unparse.Problem.Rule (unparseCopsRules)
import TRSConversion.Formats.COPS.Unparse.Problem.TrsSig (unparseCopsTrsSig)
import TRSConversion.Unparse.Utils (filterEmptyDocs)

-- | Unparse a first-order TRS from the Haskell 'Trs' representation into
-- [COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).
--
-- Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
-- unparse each part of the 'Trs'.
--
-- See the tests for examples of expected output.
unparseCopsTrs :: (Ord f, Ord v, Pretty f, Pretty v) => Trs f v -> Either String (Doc ann)
unparseCopsTrs (Trs {rules = systemMap, signature = sig, numSystems = _}) = do
  let trsUnion = concat systemMap
  prettySystems <- forM (toList systemMap) $ \rs -> do
    copsSig <- unparseCopsTrsSig trsUnion sig
    return $ vsep (filterEmptyDocs [copsSig, unparseCopsRules rs])
  pure $ vsep prettySystems

