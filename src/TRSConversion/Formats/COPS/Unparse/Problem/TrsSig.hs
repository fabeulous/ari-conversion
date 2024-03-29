{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Formats.COPS.Unparse.Problem.TrsSig
-- Description : Unparser for TrsSig
--
-- This module defines functions to unparse a 'TrsSig' into COPS and ARI format.
--
-- COPS format specifies either only variables or both variables and function symbols,
-- whereas ARI format only specifies function symbols.
-- So it is sometimes necessary to infer variables or function symbols from the
-- given signature and the rewrite system rules.
module TRSConversion.Formats.COPS.Unparse.Problem.TrsSig
  ( -- * COPS
    unparseCopsTrsSig,
  )
where

import qualified Data.Set as Set
import Prettyprinter (Doc, Pretty, emptyDoc, hsep, parens, pretty, vsep)

import TRSConversion.Problem.Common.Rule (Rule, ruleVars)
import qualified TRSConversion.Problem.Common.Rule as Rule
import TRSConversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))
import TRSConversion.Unparse.Utils (prettyBlock)


-- | Pretty print a 'TrsSig' in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php).
-- @Right doc@ indicates a success, and @Left err@ indicates an error due to a variable being in
-- both the variables and the 'FunSig' function signature.
--
-- * If the 'TrsSig' only has variables specified (via 'Vars'), then this is translated into
--      @(VAR x1 x2 ... xm)@ for each variable @xi@
--
-- * If the 'TrsSig' has variables and function symbols specified (via 'FullSig'), then this is translated into
--      @(VAR x1 ... xm)\\n(SIG (f1 a1) ... (fn ai))@ for each variable @xi@ and function symbol @fi@ with arity @ai@
--
-- * If the 'TrsSig' has only function symbols specified (via 'FunSig'), then the variables in the TRS are extracted
--      from the rules using 'ruleVars' and this case is treated like the 'FullSig' case.
--      This is the reason for rules being given as an argument @rs@.
--
-- __Important:__ does not check that the signature for duplicates, overlaps between variables and
--   function symbols, consistency with rules, etc. This should be done separately.
unparseCopsTrsSig :: (Ord f, Ord v, Pretty f, Pretty v) => [Rule f v] -> TrsSig f -> Either String (Doc ann)
unparseCopsTrsSig rs (FunSig fs)
    | Set.fromList [f | Sig f _ <- fs] `Set.isSubsetOf` Set.fromList (Rule.ruleFuns rs)
      = Right $ prettyVars variables
    | otherwise = Right $ vsep [prettyNonEmptyVars variables, prettyCopsSig fs]
  where
    variables = ruleVars rs

    prettyVars, prettyNonEmptyVars :: Pretty v => [v] -> Doc ann
    prettyVars vs = if null vs then emptyDoc else prettyNonEmptyVars vs
    prettyNonEmptyVars vs = prettyBlock "VAR" (hsep $ map pretty vs)

    prettyCopsSig :: Pretty f => [Sig f] -> Doc ann
    prettyCopsSig sig = prettyBlock "SIG" (hsep $ map (parens . pretty) sig)
