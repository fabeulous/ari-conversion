{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Unparse.UnparseMsTrs
-- Description : Unparser for MSTRSs
--
-- This module defines functions to output an 'MsTrs' in COPS and ARI format.
module TRSConversion.Unparse.UnparseMsTrs
  ( -- * COPS
    unparseCopsMsTrs,

    -- * ARI
    unparseAriMsTrs,
  )
where

import TRSConversion.Problem.MsTrs.MsSig (inferSorts)
import TRSConversion.Problem.MsTrs.MsTrs (MsTrs (..))
import TRSConversion.Unparse.Problem.MsSig (unparseAriMsSig, unparseCopsMsSig)
import TRSConversion.Unparse.Problem.Rule (unparseCopsRules, unparseAriSystems)
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import Data.Maybe (fromMaybe)
import Prettyprinter (Doc, Pretty, pretty, vcat, vsep, parens, (<+>))
import qualified Data.IntMap as IntMap

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

-- | Unparse a many-sorted TRS from the Haskell 'MsTrs' representation into
-- [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).
--
-- Uses functions 'unparseAriMetaInfo', 'unparseAriMsSig', and 'unparseAriRules' to
-- unparse each part of the 'MsTrs'.
--
-- If no 'sorts' are known, then sorts are inferred from the signature using 'inferSorts'.
unparseAriMsTrs :: (Eq s, Pretty f, Pretty v, Pretty s) => MsTrs f v s -> Doc ann
unparseAriMsTrs (MsTrs {rules = rs, signature = sig, sorts = ss, numSystems = n}) = do
  vsep $
    filterEmptyDocs
      [ formatString,
        prettySorts $ fromMaybe (inferSorts sig) ss,
        unparseAriMsSig sig,
        unparseAriSystems rs
      ]
  where
    formatString = parens $ "format" <+> "MSTRS" <>
        if n > 1 then mempty <+> ":number" <+> pretty n else mempty

    prettySorts :: Pretty s => [s] -> Doc ann
    prettySorts sortsList = vcat $ map (prettyBlock "sort" . pretty) sortsList
