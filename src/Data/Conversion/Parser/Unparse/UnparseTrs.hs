-- |
-- Module      : Data.Conversion.Parser.Unparse.UnparseTrs
-- Description : Unparser for TRSs
--
-- This module defines functions to output a 'Trs' in COPS and ARI format.
module Data.Conversion.Parser.Unparse.UnparseTrs
  ( unparseCops,
    unparseAri,
  )
where

import Data.Conversion.Parser.Unparse.Problem.MetaInfo (unparseCopsMetaInfo)
import Data.Conversion.Parser.Unparse.Problem.Rule (unparseCopsRules)
import Data.Conversion.Parser.Unparse.Problem.TrsSig (unparseCopsTrsSig)
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Maybe (catMaybes)
import Prettyprinter (Doc, Pretty, vsep)

-- | Unparse a first-order TRS from the internal 'Trs' representation into
-- [COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).
--
-- Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
-- unparse each part of the Trs.
unparseCops :: (Eq v, Pretty f, Pretty v) => Trs f v -> Doc ann
unparseCops (Trs rs sig meta) =
  vsep $ unparseCopsTrsSig sig rs : catMaybes [unparseCopsRules rs, unparseCopsMetaInfo meta]

-- | qqjf
unparseAri :: (Pretty f, Pretty v) => Trs f v -> Doc ann
unparseAri = undefined