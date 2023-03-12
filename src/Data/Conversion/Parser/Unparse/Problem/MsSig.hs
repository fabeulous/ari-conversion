-- |
-- Module      : Data.Conversion.Parser.Unparse.Problem.MsSig
-- Description : Unparser for MsSig
--
-- This module defines functions to unparse a 'MsSig' into COPS and ARI format.
module Data.Conversion.Parser.Unparse.Problem.MsSig
  ( unparseCopsMsSig,
    unparseAriMsSig,
  )
where

import Data.Conversion.Parser.Unparse.Utils (prettyBlock)
import Data.Conversion.Problem.Mstrs.MsSig (MsSig (..))
import Prettyprinter (Doc, Pretty, hsep, parens, pretty, vsep)

-- | Pretty print an 'MsSig' in [COPS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
--
-- __Important:__ does not check that the signature for duplicates, overlaps between variables and
-- function symbols, consistency with rules, type correctness, etc. This should be done separately.
unparseCopsMsSig :: (Pretty f, Pretty s) => [MsSig f s] -> Doc ann
unparseCopsMsSig msSigs = prettyBlock "SIG" (hsep $ map prettyCopsMsSig msSigs)
  where
    prettyCopsMsSig :: (Pretty f, Pretty s) => MsSig f s -> Doc ann
    prettyCopsMsSig (MsSig fsym (inSorts, outSort)) = (parens . hsep) [pretty fsym, hsep $ map pretty inSorts, pretty "->", pretty outSort]

-- | Pretty print a an 'MsSig' in [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).
-- @Right (Doc ann@) indicates a success, and @Left err@ indicates an error due to 'sorts' being set but incomplete.
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