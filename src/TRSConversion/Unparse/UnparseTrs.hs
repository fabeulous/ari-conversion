{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Unparse.UnparseTrs
-- Description : Unparser for TRSs
--
-- This module defines functions to output a 'Trs' in COPS and ARI format.
module TRSConversion.Unparse.UnparseTrs
  ( -- * COPS
    unparseCopsTrs,

    -- * ARI
    unparseAriTrs,
  )
where

import TRSConversion.Problem.Trs.Trs (Trs (..))
import TRSConversion.Unparse.Problem.Rule (unparseCopsRules, unparseAriSystems)
import TRSConversion.Unparse.Problem.TrsSig (unparseAriTrsSig, unparseCopsTrsSig)
import TRSConversion.Unparse.Utils (filterEmptyDocs)
import Prettyprinter (Doc, Pretty, pretty, vsep, (<+>), parens)
import Control.Monad (forM)
import Data.Foldable (toList)

-- | Unparse a first-order TRS from the Haskell 'Trs' representation into
-- [COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).
--
-- Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
-- unparse each part of the 'Trs'.
--
-- See the tests for examples of expected output.
unparseCopsTrs :: (Eq v, Pretty f, Pretty v) => Trs f v -> Either String (Doc ann)
unparseCopsTrs (Trs {rules = systemMap, signature = sig, numSystems = _}) = do
  prettySystems <- forM (toList systemMap) $ \rs -> do
    copsSig <- unparseCopsTrsSig rs sig
    return $ vsep (filterEmptyDocs [copsSig, unparseCopsRules rs])
  pure $ vsep prettySystems

-- | Unparse a first-order TRS from the Haskell 'Trs' representation into
-- [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt).
--
-- Uses functions 'unparseAriMetaInfo', 'unparseAriTrsSig', and 'unparseAriRules' to
-- unparse each part of the 'Trs'.
--
-- See the tests for examples of expected output.
unparseAriTrs :: (Pretty f, Pretty v, Eq v, Eq f) => Trs f v -> Either String (Doc ann)
unparseAriTrs (Trs {rules = rs, signature = sig, numSystems = n}) = do
  ariSig <- unparseAriTrsSig (concat rs) sig
  let trsElements =
        [ parens $ "format" <+> "TRS" <> if n > 1 then mempty <+> ":number" <+> pretty n else mempty,
          ariSig,
          unparseAriSystems rs
        ]
  return $ vsep (filterEmptyDocs trsElements)
