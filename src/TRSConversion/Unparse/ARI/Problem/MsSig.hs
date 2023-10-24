{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Unparse.Problem.MsSig
-- Description : Unparser for MsSig
--
-- This module defines functions to unparse an MSTRS signature 'MsSig' into COPS and ARI format.
module TRSConversion.Unparse.Problem.MsSig
  ( -- *  ARI
    unparseAriMsSig,
  )
where

import TRSConversion.Problem.MsTrs.MsSig (MsSig (..))
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import Prettyprinter (Doc, Pretty, emptyDoc, hsep, indent, parens, pretty, vsep, (<+>))

-- | Pretty print a an 'MsSig' in [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).
--
-- See the tests for examples.
--
-- __Important:__ does not check that the signature for duplicates, overlaps between variables and
-- function symbols, consistency with rules, type correctness, etc. This should be done separately.
unparseAriMsSig :: (Pretty f, Pretty s) => [MsSig f s] -> Doc ann
unparseAriMsSig = vsep . map (prettyBlock "fun" . prettyAriMsSig)
  where
    prettyAriMsSig :: (Pretty f, Pretty s) => MsSig f s -> Doc ann
    prettyAriMsSig (MsSig fsym (inSorts, outSort)) =
      hsep
        [ pretty fsym,
          if null inSorts
          then pretty outSort
          else parens ("->" <+> hsep [pretty s | s <- inSorts] <+> pretty outSort)
        ]
