{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Unparse.Problem.TrsSig
-- Description : Unparser for TrsSig
--
-- This module defines functions to unparse a 'TrsSig' into COPS and ARI format.
--
-- COPS format specifies either only variables or both variables and function symbols,
-- whereas ARI format only specifies function symbols.
-- So it is sometimes necessary to infer variables or function symbols from the
-- given signature and the rewrite system rules.
module TRSConversion.Unparse.Problem.TrsSig
  ( -- * COPS
    unparseCopsTrsSig,

    -- * ARI
    unparseAriTrsSig,
    unparseAriSigs,
  )
where

import TRSConversion.Problem.Common.Rule (Rule, inferSigFromRules, ruleVars)
import TRSConversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))
import TRSConversion.Unparse.Utils (prettyBlock)
import Prettyprinter (Doc, Pretty, emptyDoc, hsep, parens, pretty, vsep, (<+>))
import qualified Data.Set as Set
import qualified TRSConversion.Problem.Common.Rule as Rule

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
    prettyCopsSig fs = prettyBlock "SIG" (hsep $ map (parens . pretty) fs)

-- | Pretty print a 'TrsSig' in [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt).
--   @Right doc@ indicates a success, and @Left err@ indicates an error due to being unable to deduce the signature from
--   the given rules.
--
-- * If the 'TrsSig' has only function symbols specified (via 'FunSig'), then each 'Sig' is pretty printed. Note that
--      this list not checked for duplicate function symbols.
--
-- * If the 'TrsSig' has variables and function symbols specified (via 'FullSig'), then the function symbols are pretty printed
--      as in the 'FunSig' case and the variables are ignored. Note that anything not in the function signature will be treated as a variable
--      so this could change whether new rules are valid in the TRS.
--
-- * If the 'TrsSig' only has variables specified (via 'Vars'), then the function symbols and their arities are extracted from
--      the TRS rules using 'inferSigFromRules' and output as in the 'FunSig' case.
--
-- __Important:__ does not check that the signature for duplicates, overlaps between variables and
-- function symbols, consistency with rules, etc. This should be done separately.
unparseAriTrsSig :: (Pretty f) => [Rule f v] -> TrsSig f -> Either String (Doc ann)
unparseAriTrsSig _ (FunSig fs) = Right (unparseAriSigs fs)


unparseAriSigs :: Pretty f => [Sig f] -> Doc ann
unparseAriSigs sigs = vsep $ map (\(Sig f a) -> parens $ "fun" <+> pretty f <+> pretty a) sigs
