{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Formats.ARI.Unparse.Problem.MetaInfo
-- Description : Unparser for MetaInfo
--
-- This module defines functions to unparse 'MetaInfo' into the formats expected for
-- COPS and ARI rewrite systems.
module TRSConversion.Formats.ARI.Unparse.Problem.MetaInfo
  ( -- * ARI
    unparseAriMetaInfo,
  )
where

import Prettyprinter (Doc, emptyDoc, pretty, vsep, (<+>), semi)

import TRSConversion.Problem.Common.MetaInfo (MetaInfo (..))
import TRSConversion.Unparse.Utils (filterEmptyDocs)

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
unparseAriMetaInfo :: MetaInfo -> Doc ann
unparseAriMetaInfo (MetaInfo {comment = cs, doi= ds, origin = orig, submitted = sub, copsNum = cn}) =
  if null metaBlocks
    then mempty
    else vsep metaBlocks
  where
    metaBlocks :: [Doc ann]
    metaBlocks =
      filterEmptyDocs $
        maybe [] (map (metaLine "author")) sub
        ++ [ maybe emptyDoc (metaLine "doi") ds
           , maybe emptyDoc (metaLine "origin") orig
           , maybe emptyDoc (metaLine "cops") cn
           ]
        ++ maybe [] (map commentLine) cs

    metaLine :: String -> String -> Doc ann
    metaLine name xs = semi <+> "@" <> pretty name <+> pretty xs

    commentLine s = semi <+> pretty s
