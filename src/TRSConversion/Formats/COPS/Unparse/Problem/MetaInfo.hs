{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Formats.COPS.Unparse.Problem.MetaInfo
-- Description : Unparser for MetaInfo
--
-- This module defines functions to unparse 'MetaInfo' into the formats expected for
-- COPS and ARI rewrite systems.
module TRSConversion.Formats.COPS.Unparse.Problem.MetaInfo
  ( -- * COPS
    unparseCopsMetaInfo,
  )
where

import Prettyprinter (Doc, comma, emptyDoc, hsep, pretty, punctuate, vsep, (<+>), hardline)

import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..))
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)

-- | Unparse TRS 'MetaInfo' to fit into a single COPS @COMMENT@ block.
-- If the 'MetaInfo' is empty then returns 'emptyDoc'.
--
-- qqjf I was unsure what output format is desired, but this is easy to adjust.
-- See the tests for examples of the current output format.
unparseCopsMetaInfo :: MetaInfo -> Doc ann
unparseCopsMetaInfo (MetaInfo {comment = cs, doi = ds, origin = orig, submitted = sub, copsNum = cn}) =
  if null metaBlocks
    then emptyDoc
    else prettyBlock "COMMENT" (vsep (emptyDoc : metaBlocks) <> hardline)
  where
    metaBlocks :: [Doc ann]
    metaBlocks =
      filterEmptyDocs $
        [maybe emptyDoc (\d -> " doi:" <> pretty d) ds]
        ++ filterEmptyDocs
          [ maybe emptyDoc (\org -> " origin:" <+> pretty org) orig,
            maybe emptyDoc (\n -> " cops number:" <+> pretty n) cn,
            maybe emptyDoc unparseSubmitters sub
          ]
        ++ maybe [] (map pretty) cs
    -- Unparse submitters as a comma-separated list
    unparseSubmitters :: [String] -> Doc ann
    unparseSubmitters xs = " submitted by:" <+> hsep (punctuate comma $ map pretty xs)
