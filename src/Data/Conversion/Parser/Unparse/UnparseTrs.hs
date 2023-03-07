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
import Data.Conversion.Problem.Common.Rule (Rule (..), ruleVars)
import Data.Conversion.Problem.Common.Term (Term (..))
import Data.Conversion.Problem.Trs.Sig (Sig (..))
import Data.Conversion.Problem.Trs.Trs (Trs (..))
import Data.Conversion.Problem.Trs.TrsSig (TrsSig (..))
import Data.Maybe (catMaybes, isJust)
import Data.Monoid (First (..), getFirst)
import Prettyprinter (Doc, Pretty, comma, emptyDoc, encloseSep, hsep, indent, lparen, parens, pretty, punctuate, rparen, tupled, vsep, (<+>), (<>))

-- | Unparse a first-order TRS from the internal 'Trs' representation into
-- [COPS TRS format](http://project-coco.uibk.ac.at/problems/trs.php).
--
-- Uses default show instances of function symbols of type @f@ and variables of type @v@.
unparseCops :: (Eq v, Pretty f, Pretty v) => Trs f v -> Doc ann
unparseCops (Trs rs sig meta) =
  vsep $ unparseCopsTrsSig sig rs : catMaybes [unparseCopsRules rs, unparseCopsMetaInfo meta]

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
          maybe emptyDoc (\or -> pretty "origin:" <+> pretty or) orig,
          maybe emptyDoc parseSubmitters sub
        ]
    parseSubmitters :: [String] -> Doc ann
    parseSubmitters xs = pretty "submitted by:" <+> hsep (punctuate comma $ map pretty xs)

-- Conditionally renders a block only if the 'Maybe' value is not 'Nothing'.
-- If @maybeXs@ is 'Just xs' then returns a 'prettyBlock' with name @name@ and contents @xs@,
-- otherwise returns an 'emptyDoc'.
maybePretty :: Pretty a => String -> Maybe a -> Doc ann
maybePretty name maybeXs = case maybeXs of
  Nothing -> emptyDoc
  Just xs -> prettyBlock name (pretty xs)

-- | Returns True only if showing the input 'Doc' does not result in an empty string
isEmptyDoc :: Doc ann -> Bool
isEmptyDoc = null . show

-- | Wrap a 'Doc' in parentheses with the block @name@ followed by a space.
-- Creates a block like @(name doc)@ where @doc@ is the remaining document contents.
--
-- Used as a helper for unparsing
prettyBlock :: String -> Doc ann -> Doc ann
prettyBlock name doc = parens $ pretty name <+> doc

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

-- | Unparse 'Term's using applicative notation (see examples below).
-- See 'unparsePrefixTerm' to pretty print term in prefix notation.
--
-- >>> unparseTerm [Var "x"]
-- x
--
-- >>> unparseTerm (Fun "f" [Var "x", Var "y"])
-- f(x,y)
--
-- >>> unparseTerm (Fun "c" [])
-- c
unparseTerm :: (Pretty f, Pretty v) => Term f v -> Doc ann
unparseTerm (Var x) = pretty x
unparseTerm (Fun f ts) = pretty f <> args
  where
    args
      | null ts = emptyDoc -- Parse constants without parentheses
      | otherwise = encloseSep lparen rparen comma [unparseTerm ti | ti <- ts]

-- | Pretty print a 'TrsSig' in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php).
--
-- * If the 'TrsSig' only has variables specified (via 'Vars'), then this is translated into
--      @(VAR x1 x2 ... xm)@ for each variable @xi@
--
-- * If the 'TrsSig' has variables and function symbols specified (via 'FullSig'), then this is translated into
--      @(VAR x1 ... xm)\n(SIG (f1 a1) ... (fn ai))@ for each variable @xi@ and function symbol @fi@ with arity @ai@
--
-- * If the 'TrsSig' has only function symbols specified (via 'FunSig'), then all variables in the TRS rules are extracted and this case
--      is treated like the 'FullSig' case. This is the reason for rules being given as an argument.
unparseCopsTrsSig :: (Eq v, Pretty f, Pretty v) => TrsSig f v -> [Rule f v] -> Doc ann
unparseCopsTrsSig trsSig rs = case trsSig of
  Vars vs -> prettyVars vs
  FullSig vs fs -> vsep [prettyVars vs, prettyCopsSig fs]
  FunSig fs -> unparseCopsTrsSig (FullSig (ruleVars rs) fs) rs -- Extract variables from TRS rules
  where
    prettyVars :: Pretty v => [v] -> Doc ann
    prettyVars vs = prettyBlock "VAR" (hsep $ map pretty vs)
    prettyCopsSig :: Pretty f => [Sig f] -> Doc ann
    prettyCopsSig fs = prettyBlock "SIG" (hsep $ map pretty fs)

-- | qqjf
unparseAri :: (Pretty f, Pretty v) => Trs f v -> Doc ann
unparseAri = undefined