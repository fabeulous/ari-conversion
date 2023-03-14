-- |
-- Module      : Data.Conversion.Unparse.Problem.TrsSig
-- Description : Unparser for TrsSig
--
-- This module defines functions to unparse a 'TrsSig' into COPS and ARI format.
module Data.Conversion.Unparse.Problem.TrsSig
  ( unparseCopsTrsSig,
    unparseAriTrsSig,
  )
where

import Data.Conversion.Problem.Common.Rule (Rule, inferSigFromRules, ruleVars)
import Data.Conversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))
import Data.Conversion.Unparse.Utils (prettyBlock)
import Prettyprinter (Doc, Pretty, emptyDoc, hsep, parens, pretty, vsep)

-- | Pretty print a 'TrsSig' in [COPS format](http://project-coco.uibk.ac.at/problems/trs.php).
-- @Right (Doc ann@) indicates a success, and @Left err@ indicates an error due to a variable being in
-- both the variables and the 'FunSig' function signature.
--
-- __Important:__ does not check that the signature for duplicates, overlaps between variables and
--   function symbols, consistency with rules, etc. This should be done separately.
--
-- * If the 'TrsSig' only has variables specified (via 'Vars'), then this is translated into
--      @(VAR x1 x2 ... xm)@ for each variable @xi@
--
-- * If the 'TrsSig' has variables and function symbols specified (via 'FullSig'), then this is translated into
--      @(VAR x1 ... xm)\n(SIG (f1 a1) ... (fn ai))@ for each variable @xi@ and function symbol @fi@ with arity @ai@
--
-- * If the 'TrsSig' has only function symbols specified (via 'FunSig'), then all variables in the TRS rules are extracted
--      using 'ruleVars' and this case is treated like the 'FullSig' case. This is the reason for rules being given as an argument @rs@.
unparseCopsTrsSig :: (Eq v, Pretty f, Pretty v) => [Rule f v] -> TrsSig f v -> Either String (Doc ann)
unparseCopsTrsSig rs trsSig = case trsSig of
  Vars vs -> Right $ prettyVars vs
  FullSig vs fs -> Right $ vsep [prettyNonEmptyVars vs, prettyCopsSig fs]
  FunSig fs -> unparseCopsTrsSig rs $ FullSig (ruleVars rs) fs
  where
    prettyVars, prettyNonEmptyVars :: Pretty v => [v] -> Doc ann
    prettyVars vs = if null vs then emptyDoc else prettyNonEmptyVars vs
    prettyNonEmptyVars vs = prettyBlock "VAR" (hsep $ map pretty vs)
    prettyCopsSig :: Pretty f => [Sig f] -> Doc ann
    prettyCopsSig fs = prettyBlock "SIG" (hsep $ map (parens . pretty) fs)

-- | Pretty print a 'TrsSig' in [ARI format](https://ari-informatik.uibk.ac.at/tasks/A/trs.txt).
--   @Right (Doc ann@) indicates a success, and @Left err@ indicates an error due to being unable to deduce the signature from
--   the given rules.
--
-- __Important:__ does not check that the signature for duplicates, overlaps between variables and
-- function symbols, consistency with rules, etc. This should be done separately.
--
-- * If the 'TrsSig' has only function symbols specified (via 'FunSig'), then each 'Sig' is pretty printed. Note that
--      this list not checked for duplicate function symbols in this function.
--
-- * If the 'TrsSig' has variables and function symbols specified (via 'FullSig'), then the function symbols are pretty printed
--      as in the 'FunSig' case and the variables are ignored. Note that this could change whether new rules are valid in the
--      TRS as anything not in the function signature will be treated as a variable.
--
-- * If the 'TrsSig' only has variables specified (via 'Vars'), then the function symbols and their arities are extracted from
--      the TRS rules using 'inferSigFromRules' and output as in the 'FunSig' case.
unparseAriTrsSig :: (Eq v, Eq f, Show f, Pretty f, Pretty v) => [Rule f v] -> TrsSig f v -> Either String (Doc ann)
unparseAriTrsSig _ (FunSig fs) = Right (vsep $ map (prettyBlock "fun" . pretty) fs)
unparseAriTrsSig rs (FullSig _ fs) = unparseAriTrsSig rs (FunSig fs)
unparseAriTrsSig rs (Vars _) = case inferSigFromRules rs of -- Extract signature from TRS rules
  Right fs -> unparseAriTrsSig rs (FunSig fs)
  Left err -> Left err