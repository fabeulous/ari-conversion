-- |
-- Module      : TRSConversion.Unparse.UnparseTrs
-- Description : Unparser for TRSs
--
-- This module defines functions to output a 'Trs' in COPS and ARI format.
module TRSConversion.Unparse.UnparseTrs
  ( -- * COPS
    unparseCopsTrs,

    -- * ARI
    unparseAriTrs,
  )
where

import TRSConversion.Problem.Trs.Trs (Trs (..))
import TRSConversion.Unparse.Problem.MetaInfo (unparseAriMetaInfo, unparseCopsMetaInfo)
import TRSConversion.Unparse.Problem.Rule (unparseAriRules, unparseCopsRules)
import TRSConversion.Unparse.Problem.TrsSig (unparseAriTrsSig, unparseCopsTrsSig)
import TRSConversion.Unparse.Utils (filterEmptyDocs)
import Data.Maybe (fromMaybe)
import Prettyprinter (Doc, Pretty, emptyDoc, pretty, vsep)

-- | Unparse a first-order TRS from the Haskell 'Trs' representation into
-- [COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).
--
-- Uses functions 'unparseCopsTrsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
-- unparse each part of the 'Trs'.
--
-- See the tests for examples of expected output.
unparseCopsTrs :: (Eq v, Pretty f, Pretty v) => Trs f v -> Either String (Doc ann)
unparseCopsTrs (Trs rs sig meta) = do
  copsSig <- unparseCopsTrsSig rs sig
  return $ vsep (filterEmptyDocs [copsSig, unparseCopsRules rs, unparseCopsMetaInfo meta])

-- | Unparse a first-order TRS from the Haskell 'Trs' representation into
-- [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt).
--
-- Uses functions 'unparseAriMetaInfo', 'unparseAriTrsSig', and 'unparseAriRules' to
-- unparse each part of the 'Trs'.
--
-- See the tests for examples of expected output.
unparseAriTrs :: (Pretty f, Pretty v, Eq v, Eq f, Show f) => Trs f v -> Either String (Doc ann)
unparseAriTrs (Trs rs sig meta) = do
  ariSig <- unparseAriTrsSig rs sig
  let trsElements =
        [ unparseAriMetaInfo meta,
          pretty "(format TRS)",
          ariSig,
          fromMaybe emptyDoc (unparseAriRules rs)
        ]
  return $ vsep (filterEmptyDocs trsElements)
