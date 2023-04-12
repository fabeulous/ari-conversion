-- |
-- Module      : Data.Conversion.Unparse.Utils
-- Description : Utils for unparsing
--
-- This module defines helper functions to help with unparsing using Prettyprinter.
module Data.Conversion.Unparse.Utils
  ( isEmptyDoc,
    filterEmptyDocs,
    prettyBlock,
  )
where

import Prettyprinter (Doc, parens, pretty, (<+>))

-- | Returns True only if showing the input 'Doc' results in an empty string
isEmptyDoc :: Doc ann -> Bool
isEmptyDoc = null . show

-- | Helper function to remove empty docs from a list. Uses 'isEmptyDoc'.
filterEmptyDocs :: [Doc ann] -> [Doc ann]
filterEmptyDocs = filter (not . isEmptyDoc)

-- | Wrap a 'Doc' in parentheses with the block @name@ followed by a space.
-- Creates a block like @(name doc)@ where @doc@ is the remaining document contents.
prettyBlock :: String -> Doc ann -> Doc ann
prettyBlock name doc = parens $ pretty name <+> doc
