{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Formats.ARI.Unparse.UnparseMsTrs
-- Description : Unparser for MSTRSs
--
-- This module defines functions to output an 'MsTrs' in COPS and ARI format.
module TRSConversion.Formats.ARI.Unparse.UnparseMsTrs
  ( -- * ARI
    unparseAriMsTrs,
  )
where

import Data.Maybe (fromMaybe)
import Prettyprinter (Doc, Pretty, parens, pretty, vcat, vsep, (<+>))

import TRSConversion.Problem.MsTrs.MsSig (inferSorts)
import TRSConversion.Problem.MsTrs.MsTrs (MsTrs (..))
import TRSConversion.Formats.ARI.Unparse.Problem.MsSig (unparseAriMsSig)
import TRSConversion.Formats.ARI.Unparse.Problem.Rule (unparseAriSystems)
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)


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
