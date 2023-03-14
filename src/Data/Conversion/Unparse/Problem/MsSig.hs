-- |
-- Module      : Data.Conversion.Unparse.Problem.MsSig
-- Description : Unparser for MsSig
--
-- This module defines functions to unparse an MSTRS signature 'MsSig' into COPS and ARI format.
module Data.Conversion.Unparse.Problem.MsSig
  ( -- *  COPS
    unparseCopsMsSig,

    -- *  ARI
    unparseAriMsSig,
  )
where

import Data.Conversion.Problem.MsTrs.MsSig (MsSig (..))
import Data.Conversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import Prettyprinter (Doc, Pretty, emptyDoc, hsep, indent, parens, pretty, vsep)

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
                pretty "->",
                pretty outSort
              ]
          )

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
          pretty ":sort",
          parens (hsep $ map pretty (inSorts ++ [outSort]))
        ]
