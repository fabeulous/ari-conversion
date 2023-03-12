-- |
-- Module      : Data.Conversion.Unparse.UnparseMstrs
-- Description : Unparser for MSTRSs
--
-- This module defines functions to output a 'Mstrs' in COPS and ARI format.
module Data.Conversion.Unparse.UnparseMstrs
  ( unparseCopsMstrs,
    unparseAriMstrs,
  )
where

import Data.Conversion.Problem.Mstrs.Mstrs (Mstrs (..))
import Data.Conversion.Unparse.Problem.MetaInfo (unparseAriMetaInfo, unparseCopsMetaInfo)
import Data.Conversion.Unparse.Problem.MsSig (unparseAriMsSig, unparseCopsMsSig)
import Data.Conversion.Unparse.Problem.Rule (unparseAriRules, unparseCopsRules)
import Data.Conversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import Data.Maybe (catMaybes, fromMaybe)
import Prettyprinter (Doc, Pretty, emptyDoc, pretty, vcat, vsep)

-- | Unparse a many-sorted TRS from the internal 'Mstrs' representation into
-- [COPS MSTRS format](http://project-coco.uibk.ac.at/problems/mstrs.php).
--
-- Uses functions 'unparseCopsMsSig', 'unparseCopsRules', and 'unparseCopsMetaInfo' to
-- unparse each part of the 'Trs'.
unparseCopsMstrs :: (Pretty f, Pretty v, Pretty s) => Mstrs f v s -> Either String (Doc ann)
unparseCopsMstrs (Mstrs rs sig _ meta) = do
  let trsElements = unparseCopsMsSig sig : catMaybes [unparseCopsRules rs, unparseCopsMetaInfo meta]
  return $ vsep trsElements

-- | Unparse a many-sorted TRS from the internal 'Mstrs' representation into
-- [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/mstrs.txt).
--
-- Uses functions 'unparseAriMetaInfo', 'unparseAriMsSig', and 'unparseAriRules' to
-- unparse each part of the 'Trs'.
unparseAriMstrs :: (Pretty f, Pretty v, Pretty s) => Mstrs f v s -> Doc ann
unparseAriMstrs (Mstrs rs sig ss meta) = do
  vsep $
    filterEmptyDocs
      [ fromMaybe emptyDoc (unparseAriMetaInfo meta),
        pretty "(format MSTRS)",
        prettySorts ss,
        unparseAriMsSig sig,
        fromMaybe emptyDoc (unparseAriRules rs)
      ]
  where
    prettySorts :: Pretty s => Maybe [s] -> Doc ann
    prettySorts Nothing = emptyDoc -- qqjf infer
    prettySorts (Just sortsList) = vcat $ map (prettyBlock "sort" . pretty) sortsList -- qqjf infer