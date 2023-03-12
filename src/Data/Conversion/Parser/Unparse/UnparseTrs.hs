-- |
-- Module      : Data.Conversion.Parser.Unparse.UnparseTrs
-- Description : Unparser for TRSs
--
-- This module defines functions to output a 'Trs' in COPS and ARI format.
module Data.Conversion.Parser.Unparse.UnparseTrs
  ( unparseCopsTrs,
    unparseAriTrs,
  )
where

import Data.Conversion.Parser.Unparse.Problem.MetaInfo (unparseAriMetaInfo, unparseCopsMetaInfo)
import Data.Conversion.Parser.Unparse.Problem.Rule (unparseAriRules, unparseCopsRules)
import Data.Conversion.Parser.Unparse.Problem.TrsSig (unparseAriTrsSig, unparseCopsTrsSig)
import Data.Conversion.Parser.Unparse.Utils (filterEmptyDocs)
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Maybe (catMaybes, fromMaybe)
import Prettyprinter (Doc, Pretty, emptyDoc, pretty, vsep)

-- | Unparse a first-order TRS from the internal 'Trs' representation into
-- [COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).
--
-- Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
-- unparse each part of the 'Trs'.
unparseCopsTrs :: (Eq v, Pretty f, Pretty v) => Trs f v -> Either String (Doc ann)
unparseCopsTrs (Trs rs sig meta) = do
  copsSig <- unparseCopsTrsSig sig rs
  let trsElements = copsSig : catMaybes [unparseCopsRules rs, unparseCopsMetaInfo meta]
  return $ vsep trsElements

-- | Unparse a first-order TRS from the internal 'Trs' representation into
-- [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt).
--
-- Uses functions 'unparseAriMetaInfo', 'unparseAriTrsSig', and 'unparseAriRules' to
-- unparse each part of the 'Trs'.
unparseAriTrs :: (Pretty f, Pretty v, Eq v, Eq f, Show f) => Trs f v -> Either String (Doc ann)
unparseAriTrs (Trs rs sig meta) = do
  ariSig <- unparseAriTrsSig sig rs
  return $
    vsep $
      filterEmptyDocs
        [ fromMaybe emptyDoc (unparseAriMetaInfo meta),
          pretty "(format TRS)",
          ariSig,
          fromMaybe emptyDoc (unparseAriRules rs)
        ]