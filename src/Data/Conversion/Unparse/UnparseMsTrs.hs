-- |
-- Module      : Data.Conversion.Unparse.UnparseMsTrs
-- Description : Unparser for MSTRSs
--
-- This module defines functions to output an 'MsTrs' in COPS and ARI format.
module Data.Conversion.Unparse.UnparseMsTrs
  ( -- * COPS
    unparseCopsMsTrs,

    -- * ARI
    unparseAriMsTrs,
  )
where

import Data.Conversion.Problem.MsTrs.MsSig (inferSorts)
import Data.Conversion.Problem.MsTrs.MsTrs (MsTrs (..))
import Data.Conversion.Unparse.Problem.MetaInfo (unparseAriMetaInfo, unparseCopsMetaInfo)
import Data.Conversion.Unparse.Problem.MsSig (unparseAriMsSig, unparseCopsMsSig)
import Data.Conversion.Unparse.Problem.Rule (unparseAriRules, unparseCopsRules)
import Data.Conversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import Data.Maybe (fromMaybe)
import Prettyprinter (Doc, Pretty, emptyDoc, pretty, vcat, vsep)

-- | Unparse a many-sorted TRS from the internal 'MsTrs' representation into
-- [COPS MSTRS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
--
-- Uses functions 'unparseCopsMsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
-- unparse each part of the 'MsTrs'.
unparseCopsMsTrs :: (Pretty f, Pretty v, Pretty s) => MsTrs f v s -> Either String (Doc ann)
unparseCopsMsTrs (MsTrs rs sig _ meta) =
  return $
    vsep (filterEmptyDocs [unparseCopsMsSig sig, unparseCopsRules rs, unparseCopsMetaInfo meta])

-- | Unparse a many-sorted TRS from the internal 'MsTrs' representation into
-- [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).
--
-- Uses functions 'unparseAriMetaInfo', 'unparseAriMsSig', and 'unparseAriRules' to
-- unparse each part of the 'MsTrs'.
--
-- If no 'sorts' are known, then sorts are inferred from the signature using 'inferSorts'.
unparseAriMsTrs :: (Eq s, Pretty f, Pretty v, Pretty s) => MsTrs f v s -> Doc ann
unparseAriMsTrs (MsTrs rs sig ss meta) = do
  vsep $
    filterEmptyDocs
      [ fromMaybe emptyDoc (unparseAriMetaInfo meta),
        pretty "(format MSTRS)",
        prettySorts $ fromMaybe (inferSorts sig) ss,
        unparseAriMsSig sig,
        fromMaybe emptyDoc (unparseAriRules rs)
      ]
  where
    prettySorts :: Pretty s => [s] -> Doc ann
    prettySorts sortsList = vcat $ map (prettyBlock "sort" . pretty) sortsList