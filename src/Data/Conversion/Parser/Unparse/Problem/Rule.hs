-- |
-- Module      : Data.Conversion.Parser.Unparse.Problem.Rule
-- Description : Unparser for TRS rules
--
-- This module defines functions to unparse TRS 'rules' in COPS and ARI format.
module Data.Conversion.Parser.Unparse.Problem.Rule
  ( unparseCopsRules,
  )
where

import Data.Conversion.Parser.Unparse.Problem.Term (unparseTerm)
import Data.Conversion.Parser.Unparse.Utils (prettyBlock)
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Prettyprinter (Doc, Pretty, emptyDoc, indent, pretty, vsep, (<+>))

-- | Unparse a list of 'Rule's into the expected [COPS format](http://project-coco.uibk.ac.at/problems/trs.php)
-- separated by newlines.
--
-- If no rules are given, returns @Nothing@ and otherwise returns @Just rulesBlock@.
unparseCopsRules :: (Pretty f, Pretty v) => [Rule f v] -> Maybe (Doc ann)
unparseCopsRules rs =
  if null rs
    then Nothing
    else Just $ prettyBlock "RULES" $ vsep (emptyDoc : [indent 2 $ unparseCopsRule r | r <- rs] ++ [emptyDoc])
  where
    unparseCopsRule :: (Pretty f, Pretty v) => Rule f v -> Doc ann
    unparseCopsRule (Rule l r) = unparseTerm l <+> pretty "->" <+> unparseTerm r