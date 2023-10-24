{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : TRSConversion.Formats.ARI.Unparse.Problem.TrsSig
-- Description : Unparser for TrsSig
--
-- This module defines functions to unparse a 'TrsSig' into COPS and ARI format.
--
-- COPS format specifies either only variables or both variables and function symbols,
-- whereas ARI format only specifies function symbols.
-- So it is sometimes necessary to infer variables or function symbols from the
-- given signature and the rewrite system rules.
module TRSConversion.Formats.ARI.Unparse.Problem.TrsSig
  ( -- * ARI
    unparseAriTrsSig,
    unparseAriSigs,
  )
where

import Prettyprinter (Doc, Pretty, parens, pretty, vsep, (<+>))

import TRSConversion.Problem.Common.Rule (Rule)
import TRSConversion.Problem.Trs.TrsSig (Sig (..), TrsSig (..))

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
