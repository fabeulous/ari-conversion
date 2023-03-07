-- |
-- Module      : Data.Conversion.Parser.Unparse.UnparseTrs
-- Description : Unparser for TRSs
--
-- This module defines functions to output a 'Trs' in COPS and ARI format.
module Data.Conversion.Parser.Unparse.UnparseTrs
  ( unparseCops,
    unparseAri,
  )
where

import Data.Conversion.Problem.Common.MetaInfo (MetaInfo (..))
import Data.Conversion.Problem.Common.Rule (Rule (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Prettyprinter (Doc, Pretty, emptyDoc, hsep, parens, pretty, tupled, vsep, (<+>))

-- | Unparse a first-order TRS from the internal 'Trs' representation into
-- [COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).
--
-- Uses default show instances of function symbols of type @f@ and variables of type @v@.
unparseCops :: (Show f, Show v) => Trs f v -> Doc ann
unparseCops (Trs rs sig meta) =
  unparseCopsTrsSig sig
    <+> unparseCopsRules rs
    <+> unparseCopsMetaInfo meta

unparseCopsMetaInfo :: MetaInfo -> Doc ann
unparseCopsMetaInfo (MetaInfo cs ds orig sub) = prettyBlock "COMMENT" commentBody
  where
    -- Pretty print a single 'MetaInfo' item
    prettyMetaObj :: Pretty a => String -> Maybe a -> Doc ann
    prettyMetaObj name xs = prettyBlock name $ maybe emptyDoc pretty xs
    commentBody :: Doc ann
    commentBody = vsep [prettyMetaObj "Comments:" cs, prettyMetaObj "DOI:" ds, prettyMetaObj "Origin:" ds, prettyMetaObj "Submitted by:" sub] -- qqjf

-- | Wrap a 'Doc' in parentheses with the block @name@ followed by a space.
-- Creates a block like @(name doc)@ where @doc@ is the remaining document contents.
--
-- Used as a helper for unparsing
prettyBlock :: String -> Doc ann -> Doc ann
prettyBlock name doc = parens $ pretty name <+> doc

unparseCopsRules :: (Show f, Show v) => [Rule f v] -> Doc ann
unparseCopsRules rs = vsep [pretty $ show r | r <- rs]

unparseCopsTrsSig :: (Show f, Show v) => TrsSig f v -> Doc ann
unparseCopsTrsSig trsSig = case trsSig of
  Vars vs -> prettyVars vs
  FullSig vs fs -> vsep [prettyVars vs, prettyCopsSig fs]
  FunSig s -> error "FunSig is an unsupported signature type (for now)" -- qqjf
  where
    prettyVars :: Show v => [v] -> Doc ann
    prettyVars vs = hsep $ map pretty (show vs) -- qqjf
    prettyCopsSig :: Show f => [Sig f] -> Doc ann
    prettyCopsSig fs = hsep $ map pretty (show fs) -- qqjf

-- | qqjf
unparseAri :: (Show f, Show v) => Trs f v -> Doc ann
unparseAri = undefined