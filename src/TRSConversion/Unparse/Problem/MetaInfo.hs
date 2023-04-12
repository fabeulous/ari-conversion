{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Conversion.Unparse.Problem.MetaInfo
-- Description : Unparser for MetaInfo
--
-- This module defines functions to unparse 'MetaInfo' into the formats expected for
-- COPS and ARI rewrite systems.
module Data.Conversion.Unparse.Problem.MetaInfo
  ( -- * COPS
    unparseCopsMetaInfo,

    -- * ARI
    unparseAriMetaInfo,
  )
where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..))
import Data.Conversion.Unparse.Utils (filterEmptyDocs, prettyBlock)
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

-- | Unparse 'MetaInfo' into ARI format: see the tests for more examples.
--
-- >>> unparseAriMetaInfo $ emptyMetaInfo { comment = Just "[7] Example 2", doi = Just "10.1007/11805618_6", origin = Just "COPS #20",submitted = Just ["Takahito Aoto", "Junichi Yoshida", "Yoshihito Toyama"] }
-- Returns the following contents in a 'Doc' wrapped in @Just@
--   (meta-info (origin "COPS #20"))
--   (meta-info (doi "10.1007/11805618_6"))
--   (meta-info (comment "[7] Example 2"))
--   (meta-info (submitted "Takahito Aoto" "Junichi Yoshida" "Yoshihito Toyama"))
--
-- >>> unparseAriMetaInfo emptyMetaInfo
-- Nothing
unparseAriMetaInfo :: MetaInfo -> Maybe (Doc ann)
unparseAriMetaInfo (MetaInfo cs ds orig sub) =
  if null metaBlocks
    then Nothing
    else Just $ vsep metaBlocks
  where
    metaBlocks :: [Doc ann]
    metaBlocks =
      filterEmptyDocs
        [ maybe emptyDoc (metaLine "origin") orig
        , maybe emptyDoc (metaLine "doi") ds
        ]
        ++ maybe [] (map (metaLine "author")) sub
        ++ maybe [] (map commentLine) cs

    metaLine :: String -> String -> Doc ann
    metaLine name xs = semi <+> "@" <> pretty name <+> pretty xs

    commentLine s = semi <+> pretty s
