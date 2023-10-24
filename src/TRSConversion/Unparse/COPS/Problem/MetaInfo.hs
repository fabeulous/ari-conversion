{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Unparse.COPS.Problem.MetaInfo
-- Description : Unparser for MetaInfo
--
-- This module defines functions to unparse 'MetaInfo' into the formats expected for
-- COPS and ARI rewrite systems.
module TRSConversion.Unparse.COPS.Problem.MetaInfo
  ( -- * COPS
    unparseCopsMetaInfo,
  )
where

import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..))
import TRSConversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
import Prettyprinter (Doc, comma, emptyDoc, hsep, pretty, punctuate, vsep, (<+>), semi, hardline)

-- | Unparse TRS 'MetaInfo' to fit into a single COPS @COMMENT@ block.
-- If the 'MetaInfo' is empty then returns 'emptyDoc'.
--
-- qqjf I was unsure what output format is desired, but this is easy to adjust.
-- See the tests for examples of the current output format.
unparseCopsMetaInfo :: MetaInfo -> Doc ann
unparseCopsMetaInfo (MetaInfo cs ds orig sub) =
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
            maybe emptyDoc unparseSubmitters sub
          ]
        ++ maybe [] (map pretty) cs
    -- Unparse submitters as a comma-separated list
    unparseSubmitters :: [String] -> Doc ann
    unparseSubmitters xs = " submitted by:" <+> hsep (punctuate comma $ map pretty xs)
