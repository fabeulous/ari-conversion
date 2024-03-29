{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Formats.COPS.Unparse.Problem.MsSig
-- Description : Unparser for MsSig
--
-- This module defines functions to unparse an MSTRS signature 'MsSig' into COPS and ARI format.
module TRSConversion.Formats.COPS.Unparse.Problem.MsSig
  ( -- *  COPS
    unparseCopsMsSig,
  )
where

import Prettyprinter (Doc, Pretty, emptyDoc, hsep, indent, parens, pretty, vsep)

import TRSConversion.Problem.MsTrs.MsSig (MsSig (..))
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)

-- | Pretty print an 'MsSig' in [COPS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
--
-- See the tests for examples.
--
-- __Important:__ does not check that the signature for duplicates, overlaps between variables and
-- function symbols, consistency with rules, type correctness, etc. This should be done separately.
unparseCopsMsSig :: (Pretty f, Pretty s) => [MsSig f s] -> Doc ann
unparseCopsMsSig msSigs =
  prettyBlock "SIG" $
    vsep (emptyDoc : [indent 2 $ prettyCopsMsSig s | s <- msSigs] ++ [emptyDoc])
  where
    -- Pretty print a single 'MsSig'
    prettyCopsMsSig :: (Pretty f, Pretty s) => MsSig f s -> Doc ann
    prettyCopsMsSig (MsSig fsym (inSorts, outSort)) =
      parens $
        hsep
          ( filterEmptyDocs
              [ pretty fsym,
                if null inSorts then emptyDoc else hsep $ map pretty inSorts,
                "->",
                pretty outSort
              ]
          )
