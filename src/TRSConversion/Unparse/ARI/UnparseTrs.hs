{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Unparse.ARI.UnparseTrs
-- Description : Unparser for TRSs
--
-- This module defines functions to output a 'Trs' in COPS and ARI format.
module TRSConversion.Unparse.ARI.UnparseTrs
  ( -- * ARI
    unparseAriTrs,
  )
where

import Control.Monad (forM)
import Data.Foldable (toList)
import Prettyprinter (Doc, Pretty, parens, pretty, vsep, (<+>))

import TRSConversion.Problem.Trs.Trs (Trs (..))
import TRSConversion.Unparse.ARI.Problem.Rule (unparseAriSystems)
import TRSConversion.Unparse.ARI.Problem.TrsSig (unparseAriTrsSig)
import TRSConversion.Unparse.ARI.Utils (filterEmptyDocs)

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
