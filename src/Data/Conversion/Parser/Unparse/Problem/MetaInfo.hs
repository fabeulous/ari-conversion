-- |
-- Module      : Data.Conversion.Parser.Unparse.Problem.MetaInfo
-- Description : Unparser for MetaInfo
--
-- This module defines functions to unparse 'MetaInfo' into the formats expected for the COPS and ARI formats.
module Data.Conversion.Parser.Unparse.Problem.MetaInfo
  ( unparseCopsMetaInfo,
  )
where

import Data.Conversion.Parser.Unparse.Utils (isEmptyDoc, prettyBlock)
import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..))
import Prettyprinter (Doc, comma, emptyDoc, hsep, pretty, punctuate, vsep, (<+>))

-- | Unparse TRS 'MetaInfo' to fit into a single COPS @COMMENT@ block.
-- If the 'MetaInfo' is empty then returns @Nothing@, otherwise @Just commentBlock@.
--
-- qqjf I was unsure what format is expected but this is easy to adjust.
unparseCopsMetaInfo :: MetaInfo -> Maybe (Doc ann)
unparseCopsMetaInfo (MetaInfo cs ds orig sub) =
  if null metaBlocks
    then Nothing
    else Just $ prettyBlock "COMMENT" (vsep $ emptyDoc : metaBlocks)
  where
    metaBlocks :: [Doc ann]
    metaBlocks =
      filter
        (not . isEmptyDoc)
        [ maybe emptyDoc (\d -> pretty "doi:" <> pretty d) ds, -- parseDoi "doi:" ds,
          maybe emptyDoc (vsep . map pretty) cs,
          maybe emptyDoc (\org -> pretty "origin:" <+> pretty org) orig,
          maybe emptyDoc parseSubmitters sub
        ]
    parseSubmitters :: [String] -> Doc ann
    parseSubmitters xs = pretty "submitted by:" <+> hsep (punctuate comma $ map pretty xs)