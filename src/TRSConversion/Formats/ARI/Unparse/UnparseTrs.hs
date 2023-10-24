{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Formats.ARI.Unparse.UnparseTrs
-- Description : Unparser for TRSs
--
-- This module defines functions to output a 'Trs' in COPS and ARI format.
module TRSConversion.Formats.ARI.Unparse.UnparseTrs
  ( -- * ARI
    unparseAriTrs,
  )
where

import Prettyprinter (Doc, Pretty, parens, pretty, vsep, (<+>))

import TRSConversion.Problem.Trs.Trs (Trs (..))
import TRSConversion.Formats.ARI.Unparse.Problem.Rule (unparseAriSystems)
import TRSConversion.Formats.ARI.Unparse.Problem.TrsSig (unparseAriTrsSig)
import TRSConversion.Unparse.Utils (filterEmptyDocs)

-- | Unparse a first-order TRS from the Haskell 'Trs' representation into
-- [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt).
--
-- Uses functions 'unparseAriMetaInfo', 'unparseAriTrsSig', and 'unparseAriRules' to
-- unparse each part of the 'Trs'.
--
-- See the tests for examples of expected output.
unparseAriTrs :: (Pretty f, Pretty v) => Trs f v -> Either String (Doc ann)
unparseAriTrs (Trs {rules = rs, signature = sig, numSystems = n}) = do
  ariSig <- unparseAriTrsSig (concat rs) sig
  let trsElements =
        [ parens $ "format" <+> "TRS" <> if n > 1 then mempty <+> ":number" <+> pretty n else mempty,
          ariSig,
          unparseAriSystems rs
        ]
  return $ vsep (filterEmptyDocs trsElements)
