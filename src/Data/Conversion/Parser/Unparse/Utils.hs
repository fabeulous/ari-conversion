-- |
-- Module      : Data.Conversion.Parser.Unparse.Utils
-- Description : Utils for unparsing
--
-- This module defines helper functions to help with unparsing using Prettyprinter.
module Data.Conversion.Parser.Unparse.Utils
  ( isEmptyDoc,
    prettyBlock,
    filterEmptyDocs,
  )
where

import Prettyprinter (Doc, parens, pretty, (<+>))

-- | Returns True only if showing the input 'Doc' does not result in an empty string
isEmptyDoc :: Doc ann -> Bool
isEmptyDoc = null . show

-- | Wrap a 'Doc' in parentheses with the block @name@ followed by a space.
-- Creates a block like @(name doc)@ where @doc@ is the remaining document contents.
--
-- Used as a helper for unparsing
prettyBlock :: String -> Doc ann -> Doc ann
prettyBlock name doc = parens $ pretty name <+> doc

-- | Helper function to remove empty docs from a list
filterEmptyDocs :: [Doc ann] -> [Doc ann]
filterEmptyDocs = filter (not . isEmptyDoc)